#include <Rcpp.h>
#include <vector>
#include <valarray>
#include <tuple>
#include <functional>
#include <algorithm>
#include <math.h>
#include "rcpp_utils.cpp"
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

struct Region {
    long start, end, value;
    Region (): start(0), end(0), value(0) {}
    Region (long s, long e, long v):
        start(s), end(e), value(v) {}
    bool empty () {
        return (start == 0);
    }
    long getSize () {
        return end - start + 1;
    }
    long getDifference (Region &r) {
        if ((empty()) || (r.empty()))
            return std::numeric_limits<long>::max();
        return ((start >= r.start)? start - r.start: r.start - start) +
               ((end   >= r.end  )? end   - r.end  : r.end   - end);
    }
    bool operator< (const Region& r) const {
        return (((start < r.start) ||
            ((start == r.start) && (end < r.end)) ||
            ((start == r.start) && (end < r.end) && (value < r.value))));
    }
};

double computeMedian(std::vector < std::pair < double, int > > &table){
    sort(table.begin(), table.end());
    int i, s = 0, size = 0;
    for (auto &i: table) {
            size += i.second;
    }
    for (i = 0; s < size/2; ++i) {
            s += table[i].second;
    }
    return table[i].first;
}

//' Normalize counts (and changes the input values)
//'
//' @param lengths          the sizes of the RLEs (one list per chromosome)
//' @param values           the values of the RLEs (one list per chromosome)
//' @param chromosomeSizes  the sizes of the chromosomes
//' @param librarySizes     number of elements per sample
//' @return                 nothing (but transform the values instead)
// [[Rcpp::export]]
void rcpp_normalization(ListOf < ListOf < IntegerVector > > &lengths,
                        ListOf < ListOf < IntegerVector > > &values,
                        IntegerVector &chromosomeSizes,
                        IntegerVector &librarySizes) {
    GenomeIterator iterator (lengths, values, chromosomeSizes);
    std::vector < std::pair < std::valarray < double >, int > >
        normValues;
    std::vector < std::pair < double, int > > normValuesPerSample;
    std::vector < std::pair < double, int > > avgValues;
    std::vector < std::pair < double, int > > avgNormValues;
    int nSamples     = lengths[0].size();
    int nChromosomes = chromosomeSizes.size();
    std::valarray < double > theseValues (nSamples);
    std::valarray < double > factors (nSamples);
    std::valarray < double > sums (nSamples);
    std::valarray < double > normalizedSums (nSamples);
    std::valarray < double > librarySizesArray (nSamples);
    for (int i = 0; i < nSamples; ++i) {
        librarySizesArray[i] = librarySizes[i];
    }
    for (; ! iterator.isOver(); iterator.getNext()) {
        theseValues = iterator.getValuesDouble();
        if (theseValues.min() > 1) {
            theseValues /= exp(log(theseValues).sum() / nSamples);
            normValues.emplace_back(theseValues, iterator.getStep());
        }
    }
    for (int sample = 0; sample < nSamples; ++sample) {
        normValuesPerSample.clear();
        normValuesPerSample.reserve(normValues.size());
        for (auto &normValue: normValues) {
            normValuesPerSample.emplace_back((normValue.first)[sample],
                                             normValue.second);
        }
        factors[sample] = computeMedian(normValuesPerSample) /
            librarySizes[sample];
    }
    factors           /= exp(factors.sum() / nSamples);
    librarySizesArray *= factors;
    for (iterator.reset(); ! iterator.isOver(); iterator.getNext()) {
        sums           += iterator.getValuesDouble();
        normalizedSums += (iterator.getValuesDouble() / librarySizesArray);
    }
    std::nth_element(std::begin(sums), std::begin(sums) + nSamples / 2,
                     std::end(sums));
    std::nth_element(std::begin(normalizedSums),
                     std::begin(normalizedSums) + nSamples / 2,
                     std::end(normalizedSums));
    double fs     = sums[nSamples / 2] / normalizedSums[nSamples / 2];
    for (int chromosomeId = 0; chromosomeId < nChromosomes; ++chromosomeId) {
        for (int sampleId = 0; sampleId < nSamples; ++sampleId) {
            for (int i = 0; i < values[chromosomeId][sampleId].size(); ++i) {
                values[chromosomeId][sampleId][i] =
                    static_cast<int>(round(static_cast<double>(
                            values[chromosomeId][sampleId][i]) * fs));
            }
        }
    }
}

//' Compute unique counts.
//'
//' @param lengths          the sizes of the RLEs (one list per chromosome)
//' @param values           the values of the RLEs (one list per chromosome)
//' @param chromosomeSizes  the sizes of the chromosomes
//' @param minDepth         minimum coverage
//' @param minSize          minimum region size
//' @param maxSize          maximum region size
//' @param minDifference    minimum difference between 2 regions
//' @return                 selected regions
// [[Rcpp::export]]
List rcpp_slice(ListOf < ListOf < IntegerVector > > &lengths,
                   ListOf < ListOf < IntegerVector > > &values,
                   IntegerVector &chromosomeSizes, int minDepth,
                   int minSize, int maxSize, int minDifference) {
    static const short nConditions = 2;
    std::vector < int > starts, ends;
    std::vector < std::string > chromosomes;
    int nChromosomes = chromosomeSizes.size();
    std::vector < Region > selectedRegions[nConditions];
    std::string chromosome;
    for (int chrId = 0; chrId < nChromosomes; ++chrId) {
        chromosome = as < std::string >(as < CharacterVector> (
            chromosomeSizes.names())[chrId]);
        for (int condition = 0; condition < nConditions; ++condition) {
            std::vector < Region > theseRegions;
            int  nValues = lengths[condition][chrId].size();
            long start   = 1;
            int  rLast   = 0;
            selectedRegions[condition].clear();
            for (int index = 0; index < nValues; ++index) {
                long length    = lengths[condition][chrId][index];
                long value     = values[condition][chrId][index];
                long end       = start + length - 1;
                int  nextRLast = theseRegions.size();
                for (int rit = theseRegions.size()-1; rit >= rLast; --rit) {
                    Region &region = theseRegions[rit];
                    if ((region.end == start - 1) && (region.value <= value)) {
                        region.end = end;
                        nextRLast = rit;
                    }
                }
                rLast = nextRLast;
                if (value >= minDepth) {
                    theseRegions.emplace_back(start, end, value);
                }
                start = end + 1;
            }
            for (Region &region: theseRegions) {
                long size = region.getSize();
                if ((minSize <= size) && (size <= maxSize)) {
                    long difference = minDifference + 1;
                    for (auto rit = selectedRegions[condition].rbegin();
                         rit != selectedRegions[condition].rend(); ++rit) {
                        long d = region.getDifference(*rit);
                        difference = std::min<long>(difference, d);
                        if (d > 10 * minDifference) {
                            break;
                        }
                    }
                    if (difference >= minDifference) {
                        selectedRegions[condition].push_back(region);
                    }
                }
            }
        }
        std::vector < Region > mergedRegions;
        mergedRegions.reserve(selectedRegions[0].size() +
            selectedRegions[1].size());
        std::vector < Region >::iterator its[2] =
            {selectedRegions[0].begin(), selectedRegions[1].begin()};
        while ((its[0] != selectedRegions[0].end()) &&
               (its[1] != selectedRegions[1].end())) {
            int condition = (*its[0] < *its[1])? 0: 1;
            mergedRegions.push_back(*its[condition]);
            ++its[condition];
        }
        for (int condition = 0; condition < nConditions; ++condition) {
            while (its[condition] != selectedRegions[condition].end()) {
                mergedRegions.push_back(*its[condition]);
                ++its[condition];
            }
        }
        Region previousRegion;
        for (Region &region: mergedRegions) {
            if (region.getDifference(previousRegion)) {
                starts.push_back(region.start);
                ends.push_back(region.end);
                chromosomes.push_back(chromosome);
                previousRegion = region;
            }
        }
    }
    return DataFrame::create(_["seqnames"] = chromosomes,
                             _["start"]    = starts ,
                             _["end"]      = ends);
}
