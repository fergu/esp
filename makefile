INCDIR = ./include
ESPDIR = ./esp
ESPCAMDIR = ./espcam
ESPJANDIR = ./espjan

BINDIR = ./bin
OBJDIR = ./obj
SRCDIR = ./src

ESPNAME = ESP
ESPCAMNAME = ESPCAM
ESPJANNAME = ESPJAN

ESPSETUPFILES = S0.f S1.f S2.f S3.f S4.f S5.f S6.f S7.f S8.f S9.f
ESPJANSETUPFILES = A0.f A1.f A2.f A3.f A4.f A5.f A6.f

INCFILES = ESPJWORK.h INTWRKZZ.h SCKIOZZ.h
CYCLEFILES = C1.f C2.f C3.f C4.f C5.f C6.f C7.f C8.f C9.f C10.f C11.f
DISPLAYFILES = D1.f
MANIFOLDFILES = Y1.f Y2.f Y3.f Y4.f Y5.f Y6.f Y7.f Y8.f Y9.f Y10.f
INTERFACEFILES = Ib.f I1.f I2.f I3.f
CHEMFILES = E1.f E2.f E3.f E4.f E5.f E6.f E7.f
TPFILES = T1.f T2.f
VALVEFILES = V1.f V2.f

ESPFILES = $(ESPSETUPFILES) $(CYCLEFILES) $(DISPLAYFILES) $(MANIFOLDFILES) $(INTERFACEFILES)
ESPJANFILES = $(ESPJANSETUPFILES) $(CHEMFILES) $(INTERFACEFILES) $(TPFILES)
ESPCAMFILES = $(VALVEFILES) $(INTERFACEFILES)

FCC = gfortran
FFLAGS = -I$(INCDIR)
.PHONY: clean all

all: esp espjan espcam

$(OBJDIR)/%.o: $(SRCDIR)/%.f
	$(FCC) -c -o $@ $< $(FFLAGS)

esp: $(patsubst %.f,$(OBJDIR)/%.o,$(ESPFILES))
	$(FCC) -o $(BINDIR)/$(ESPNAME) $^ $(FFLAGS)

espjan: $(patsubst %.f,$(OBJDIR)/%.o,$(ESPJANFILES))
	$(FCC) -o $(BINDIR)/$(ESPJANNAME) $^ $(FFLAGS)

espcam: $(patsubst %.f,$(OBJDIR)/%.o,$(ESPCAMFILES))
	$(FCC) -o $(BINDIR)/$(ESPCAMNAME) $^ $(FFLAGS)

clean:
	rm -f $(OBJDIR)/*.o
	rm -f $(BINDIR)/*
	rmdir $(OBJDIR) $(BINDIR)

$(shell mkdir -p $(BINDIR) $(OBJDIR))
