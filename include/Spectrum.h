#ifndef ___n_Spectrum_h__
#define ___n_Spectrum_h__

#include <complex>
#include <unistd.h>
#include <fftw3.h>

namespace bondi {
    class Spectrum {
    public:
        Spectrum(size_t fft_size = 16384, double sample_rate = 44100.0);
        ~Spectrum();
        
        std::complex<double> at(int i);
        std::complex<double> at(double freq);
        
        std::complex<double> operator[](int i);
        
        int bin(double freq);
        double freq(int bin);
        
        template <typename T>
        size_t feed(size_t n, T *samples) {
            if (n <= 0) return 0;
            
            if (n < fft_in_size) {
                memcpy(fft_in + n, fft_in, n * sizeof(double));
            } else {
                n = fft_in_size;
            }
            
            for (size_t i = 0; i < n; i++) {
                fft_in[i] = samples[i];
            }
            
            unprocessed += n;
            if (unprocessed > fft_in_size) unprocessed = fft_in_size;
            
            return n;
        }
        
    private:
        fftw_plan plan;
        size_t unprocessed;
        
        size_t fft_in_size;
        double *fft_in;
        
        size_t fft_out_size;
        fftw_complex *fft_out;
        
        double bin_size;
    };
    
}

#endif /* ___n_Spectrum_h__ */
