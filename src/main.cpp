#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#include <iostream>
#include "Spectrum.h"

const int SAMPLE_RATE = 44100;
const int CAPTUREBUF_SIZE = SAMPLE_RATE/25;

int main() {
    bondi::Spectrum s;
    ALCdevice *mic;
    int16_t buf[CAPTUREBUF_SIZE];
    
    int bin = s.bin(440);
    double freq = s.freq(bin);
    
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
            
            s.feed(samples_available, buf);
            std::cout << freq << ": " << abs(s[bin]) << std::endl;
        }
    }
    alcCaptureStop(mic);
    alcCaptureCloseDevice(mic);
    return 0;
}