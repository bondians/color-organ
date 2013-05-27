#include "Spectrum.h"
#include <limits>

namespace bondi {
    Spectrum::Spectrum(size_t fft_size, double sample_rate) :
        fft_in_size(fft_size),
        fft_out_size(fft_size/2 + 1),
        bin_size(sample_rate / (double) fft_size)
    {
        fft_in  = (double *)       fftw_malloc(fft_in_size * sizeof(double));
        fft_out = (fftw_complex *) fftw_malloc(fft_out_size * sizeof(fftw_complex));
        plan = fftw_plan_dft_r2c_1d(fft_in_size, fft_in, fft_out, FFTW_MEASURE);
        
        for (int i = 0; i < fft_in_size; i++) {
            fft_in[i] = 0.0;
        }
        
        unprocessed = fft_size;
    }
    
    Spectrum::~Spectrum() {
        fftw_destroy_plan(plan);
        fftw_free(fft_in);
        fftw_free(fft_out);
    }
    
    std::complex<double> Spectrum::at(int bin) {
        if (bin < 0 || bin >= fft_out_size) return std::numeric_limits<double>::quiet_NaN();
        
        if (unprocessed) {
            fftw_execute(plan);
            unprocessed = 0;
        }
        
        return std::complex<double>(fft_out[bin][0], fft_out[bin][1]);
    }
    
    std::complex<double> Spectrum::at(double freq) {
        return this->at(bin(freq));
    }
    
    std::complex<double> Spectrum::operator[](int i) {
        return this->at(i);
    }
    
    int Spectrum::bin(double freq) {
        return floor(0.5 + freq / bin_size);
    }
    
    double Spectrum::freq(int bin) {
        return bin * bin_size;
    }
}
