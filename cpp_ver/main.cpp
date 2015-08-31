#include <iostream>
#include <vector>
#include <map>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

using std::vector;
using std::map;
using std::make_pair;

typedef uint32_t uint;

#define GetA(c) ((c>>6)&0x7)
#define GetB(c) ((c>>3)&0x7)
#define GetC(c) (c&0x7)
#define GetR(c) ((c>>25)&0x7)
#define GetV(c) (c&0x1FFFFFF)

class UM {
    uint ip;
    uint reg[8];
    map<uint,vector<uint>*> memory;
    vector<uint> *memzero;
    void CMov(uint A, uint B, uint C) {
        if (reg[C] != 0) reg[A] = reg[B];
        ++ip;
    }
    void ArrGet(uint A, uint B, uint C){
        reg[A] = memory[reg[B]]->at(reg[C]);
        ++ip;
    }
    void ArrSet(uint A, uint B, uint C){
        memory[reg[A]]->at(reg[B]) = reg[C];
        ++ip;
    }
    void Add(uint A, uint B, uint C) {
        reg[A] = reg[B] + reg[C];
        ++ip;
    }
    void Mul(uint A, uint B, uint C) {
        reg[A] = reg[B] * reg[C];
        ++ip;
    }
    void Div(uint A, uint B, uint C) {
        reg[A] = reg[B] / reg[C];
        ++ip;
    }
    void NAnd(uint A, uint B, uint C) {
        reg[A] = ~( reg[B] & reg[C] );
        ++ip;
    }
    void Alloc(uint B, uint C) {
        uint idx = (uint)memory.size();
        while (memory.find(idx) != memory.end()) ++idx;
        memory.insert(make_pair(idx, new vector<uint>(reg[C])));
        reg[B] = idx;
        ++ip;
    }
    void Free(uint C) {
        auto x = memory.find(reg[C]);
        delete x->second;
        memory.erase(x);
        ++ip;
    }
    void Output(uint C) {
        putchar((int)reg[C]);
        fflush(stdout);
        ++ip;
    }
    void Input(uint C) {
        reg[C] = (uint)getchar();
        ++ip;
    }
    void LoadProg(uint B, uint C) {
        if (reg[B] != 0) {
            memory.erase(0);
            delete memzero;
            memzero = new vector<uint>(*memory[reg[B]]);
            memory.insert(make_pair(0, memzero));
        }
        ip = reg[C];
    }


public:
    ~UM() {
        for(auto a:memory) {
            delete(a.second);
        }
        memory.clear();
    }
    UM(vector<uint> *base):ip(0) {
        memset(reg, 0, sizeof(reg));
        memzero = base;
        memory.insert(make_pair(0, memzero));
    }
    bool Step() {
        uint code = (*memzero)[ip];
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
    vector<uint> *data = new vector<uint>(pos/sizeof(uint));
    printf("reading %li bytes\n", pos);
    long rd = read( fh, &((*data)[0]), pos);
    if (rd < 0) {
        printf("Read failed: %i\n", errno);
        close(fh);
        return 1;
    }
    close(fh);

    printf("preparing data\n");
    for (int i=0; i<data->size(); ++i) {
        uint &x=data->at((uint)i);
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