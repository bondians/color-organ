#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#include <math.h>
#include <stdio.h>
#include "Spectrum.h"

#define SAMPLE_RATE     44100
#define CAPTUREBUF_SIZE (SAMPLE_RATE/25)

int main() {
    spectrum_t *s = spectrum_init(16384, 44100.0);
    ALCdevice *mic;
    int16_t buf[CAPTUREBUF_SIZE];
    
    int bin = spectrum_bin(s, 440);
    double freq = spectrum_freq(s, bin);
    
    mic = alcCaptureOpenDevice(NULL, SAMPLE_RATE, AL_FORMAT_MONO16, CAPTUREBUF_SIZE);
    if (alGetError() != AL_NO_ERROR) {
        return 0;
    }
    
    alcCaptureStart(mic);
    while (1) {
        ALint samples_available;
        alcGetIntegerv(mic, ALC_CAPTURE_SAMPLES, (ALCsizei)sizeof(ALint), &samples_available);
        
        if (samples_available) {
            alcCaptureSamples(mic, (ALCvoid *)buf, samples_available);
            
            spectrum_feed(s, samples_available, buf);
            printf("%g: %g\n", freq, log(cabs(spectrum_get(s)[bin])));
        }
    }
    alcCaptureStop(mic);
    alcCaptureCloseDevice(mic);
    
    spectrum_destroy(s);
    return 0;
}