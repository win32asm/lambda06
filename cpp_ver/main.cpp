#include <iostream>
#include <algorithm>
#include <vector>
#include <list>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

using std::vector;
using std::list;

typedef uint32_t uint;

#define GetA(c) ((c>>6)&0x7)
#define GetB(c) ((c>>3)&0x7)
#define GetC(c) (c&0x7)
#define GetR(c) ((c>>25)&0x7)
#define GetV(c) (c&0x1FFFFFF)

class UM {
    uint ip;
    uint reg[8];
    vector<vector<uint> *> memory;
    vector<uint> *zeromem;
    list<uint> freeidx;

    void CMov(const uint A, const uint B, const uint C) {
        if (reg[C] != 0) reg[A] = reg[B];
        ++ip;
    }
    void ArrGet(const uint A, const uint B, const uint C){
        reg[A] = (*memory[reg[B]])[reg[C]];
        ++ip;
    }
    void ArrSet(const uint A, const uint B, const uint C){
        (*memory[reg[A]])[reg[B]] = reg[C];
        ++ip;
    }
    void Add(const uint A, const uint B, const uint C) {
        reg[A] = reg[B] + reg[C];
        ++ip;
    }
    void Mul(const uint A, const uint B, const uint C) {
        reg[A] = reg[B] * reg[C];
        ++ip;
    }
    void Div(const uint A, const uint B, const uint C) {
        reg[A] = reg[B] / reg[C];
        ++ip;
    }
    void NAnd(const uint A, const uint B, const uint C) {
        reg[A] = ~( reg[B] & reg[C] );
        ++ip;
    }
    void Alloc(const uint B, const uint C) {
        uint idxN;
        if (freeidx.empty()) {
            idxN = (uint)memory.size();
            memory.push_back(new vector<uint>(reg[C]));
        }else{
            idxN = freeidx.front();
            freeidx.pop_front();
            memory[idxN] = new vector<uint>(reg[C]);
        }
        reg[B] = idxN;
        ++ip;
    }
    void Free(const uint C) {
        auto const rC = reg[C];
        delete memory[rC];
        memory[rC] = nullptr;
        freeidx.push_back(rC);
        ++ip;
    }
    void Output(const uint C) {
        putchar((int)reg[C]);
        fflush(stdout);
        ++ip;
    }
    void Input(const uint C) {
        reg[C] = (uint)getchar();
        ++ip;
    }
    void LoadProg(const uint B, const uint C) {
        auto const rB = reg[B];
        if (rB != 0) {
            delete zeromem;
            memory[0] = new vector<uint>(*memory[rB]);
            zeromem = memory[0];
        }
        ip = reg[C];
    }

public:
    ~UM() {
        printf("final memory reservation: %li\n", memory.size());
        for(auto vec:memory) {
            if (vec) delete(vec);
        }
        memory.clear();
    }
    UM(const vector<uint> &base):ip(0) {
        memset(reg, 0, sizeof(reg));

        memory.push_back(new vector<uint>(base));
        zeromem = memory[0];
    }

    bool Step() {
        uint code = (*zeromem)[ip];
        switch (code >> 28) {
            case 0: CMov (GetA(code), GetB(code), GetC(code)); break;
            case 1: ArrGet (GetA(code), GetB(code), GetC(code)); break;
            case 2: ArrSet (GetA(code), GetB(code), GetC(code)); break;
            case 3: Add (GetA(code), GetB(code), GetC(code)); break;
            case 4: Mul (GetA(code), GetB(code), GetC(code)); break;
            case 5: Div (GetA(code), GetB(code), GetC(code)); break;
            case 6: NAnd (GetA(code), GetB(code), GetC(code)); break;
            case 8: Alloc(GetB(code), GetC(code)); break;
            case 9: Free(GetC(code)); break;
            case 10:Output(GetC(code)); break;
            case 11:Input(GetC(code)); break;
            case 12:LoadProg(GetB(code), GetC(code)); break;
            case 13:
                reg[GetR(code)]=GetV(code);
                ++ip;
                break;
            default:
                return false;
        }
        return true;
    }
};

int main(int argc, char **argv) {
    int fh;
    if (argc < 2) {
        printf("Not enough args\n");
        return 0;
    }
    fh = open(argv[1], O_RDONLY);
    if (fh < 0) {
        printf("Open failed: %i\n", errno);
        return 1;
    }
    off_t pos = lseek(fh, 0, SEEK_END);
    lseek(fh, 0, SEEK_SET);
    vector<uint> data(pos/sizeof(uint));
    printf("reading %li bytes\n", pos);
    long rd = read( fh, &(data[0]), pos);
    if (rd < 0) {
        printf("Read failed: %i\n", errno);
        close(fh);
        return 1;
    }
    close(fh);

    printf("preparing data\n");
    for (int i=0; i<data.size(); ++i) {
        uint &x=data[(uint)i];
        x=((x&0xFF)<<24)+((x&0xFF00)<<8)+((x&0xFF0000)>>8)+(x>>24);
    }

    UM x(data);

    printf("starting %s\n", argv[1]);
    unsigned long steps=0;
    while (x.Step()) {
        ++steps;
    }

    printf("%li steps done\n", steps);
    return 0;
}