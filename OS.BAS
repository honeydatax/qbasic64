'I2C BUS CONDITIONS
DECLARE SUB SelectI2CprinterPort (PrinterNo%)
DECLARE SUB I2CBusNotBusy ()
DECLARE SUB I2CmasterClockpulse ()
DECLARE FUNCTION I2Cinput% ()
DECLARE SUB I2Cstart ()
DECLARE SUB I2Coutput (Serdata%)
DECLARE SUB I2Cclockpulse ()
DECLARE SUB I2Cstop ()
DECLARE FUNCTION SHL% (Dec%, Positions%)

'RADIX CONVERSION FUNCTIONS
DECLARE FUNCTION DecToBin$ (DecNumber%)
DECLARE FUNCTION BinToDec% (BinNumber$)

'8 BIT AD CONVERTER PROCEDURES
DECLARE SUB ReadADchannel (ChannelNo%)

'WEATHER SUBROUTINES
DECLARE SUB ScreenLayout ()
DECLARE SUB Process ()
DECLARE SUB initialize ()

'COMMON USED CONSTANTS AND VARIABLES
CONST MaxIOcard% = 3
CONST MaxADchannel% = 16

COMMON SHARED StatusPort%, ControlPort%, I2Cbusdelay%
COMMON SHARED Temp#, TempMin#, TempMax#, TempAverage#, n%

DIM SHARED ADDAchipCode%(0 TO MaxIOcard%)
	FOR ChipNo% = 0 TO MaxIOcard%
		ADDAchipCode%(ChipNo%) = 144 + 2 * ChipNo%
	NEXT

DIM SHARED AD%(1 TO MaxADchannel%)

DIM pos1
pos1 = 0
DIM sp$
sp$ = "                       "
'example  Weather_station

'-----{main program}
SelectI2CprinterPort 1
I2Cbusdelay% = 10
I2CBusNotBusy
ScreenLayout
Process
END
'-----{main program}

FUNCTION BinToDec% (BinNumber$)
	Weight% = 1
	Dec% = 0                       'Reset decimal number

	IF BinNumber$ <> "00000000" THEN
		FOR I% = LEN(BinNumber$) TO 1 STEP -1
			IF MID$(BinNumber$, I%, 1) = "1" THEN
				Dec% = Dec% + Weight%  'If bit=1 then add weigth factor
			END IF
			Weight% = Weight% * 2  'Multiply weight factor by 2
		NEXT
		BinToDec% = Dec%        'Store result
	ELSE
		BinToDec% = 0
	END IF
END FUNCTION

' RADIX CONVERSION SUBROUTINES
FUNCTION DecToBin$ (DecNumber%)
	'Conversion of decimal number (0...255) to 8 bit binary string.
	'--------------------------------------------------------------

	Bin$ = ""
	faktor% = 128

	IF DecNumber% <> 0 THEN
		FOR I% = 1 TO 8
			IF faktor% > DecNumber% THEN
				Bin$ = Bin$ + "0"
			ELSE
				Bin$ = Bin$ + "1"
				DecNumber% = DecNumber% - faktor%
			END IF
			faktor% = faktor% \ 2
		NEXT
		DecToBin$ = Bin$
	ELSE
		DecToBin$ = "00000000"
	END IF
END FUNCTION

SUB I2CBusNotBusy
	OUT ControlPort%, 4
END SUB

SUB I2Cclockpulse
	OUT ControlPort%, 12
	FOR I% = 0 TO I2Cbusdelay%
	NEXT

	OUT ControlPort%, 4
	FOR I% = 0 TO I2Cbusdelay%
	NEXT

	OUT ControlPort%, 12
	FOR I% = 0 TO I2Cbusdelay%
	NEXT
END SUB

FUNCTION I2Cinput%
	Serdata% = 0

	FOR j% = 1 TO 8
		Serdata% = SHL(Serdata%, 1)
		OUT ControlPort%, 4
		FOR I% = 0 TO I2Cbusdelay%
		NEXT

		Inputdata% = INP(StatusPort%) AND 16
		IF Inputdata% <> 0 THEN
			Serdata% = Serdata% OR 1
		END IF

		OUT ControlPort%, 12
		FOR I% = 0 TO I2Cbusdelay%
		NEXT
	NEXT
	I2Cinput% = Serdata%
END FUNCTION

SUB I2CmasterClockpulse
	OUT ControlPort%, 14
	FOR I% = 0 TO I2Cbusdelay%
	NEXT

	OUT ControlPort%, 6
	FOR I% = 0 TO I2Cbusdelay%
	NEXT

	OUT ControlPort%, 14
	FOR I% = 0 TO I2Cbusdelay%
	NEXT

	OUT ControlPort%, 12
	FOR I% = 0 TO I2Cbusdelay%
	NEXT
END SUB

SUB I2Coutput (Serdata%)
	Temp% = Serdata%
	Serdat$ = DecToBin(Temp%)

	FOR j% = 1 TO 8

		IF MID$(Serdat$, j%, 1) = "1" THEN
			DataOut% = 12
		ELSE
			DataOut% = 14
		END IF
		OUT ControlPort%, DataOut%
		FOR I% = 0 TO I2Cbusdelay%
		NEXT

		DataOut% = INP(ControlPort%) AND 7
		OUT ControlPort%, DataOut%
		FOR I% = 0 TO I2Cbusdelay%
		NEXT

		DataOut% = INP(ControlPort%) OR 8
		OUT ControlPort%, DataOut%
		FOR I% = 0 TO I2Cbusdelay%
		NEXT
	NEXT
END SUB

SUB I2Cstart
	OUT ControlPort%, 6
	FOR I% = 0 TO I2Cbusdelay%
	NEXT

	OUT ControlPort%, 14
	FOR I% = 0 TO I2Cbusdelay%
	NEXT
END SUB

SUB I2Cstop
	OUT ControlPort%, 14
	FOR I% = 0 TO I2Cbusdelay%
	NEXT

	OUT ControlPort%, 6
	FOR I% = 0 TO I2Cbusdelay%
	NEXT

	OUT ControlPort%, 4
	FOR I% = 0 TO I2Cbusdelay%
	NEXT
END SUB

SUB initialize
    ReadADchannel 1
    Temp# = (AD%(1) - 50) / 2
    TempMin# = Temp
    TempMax# = Temp
    TempAverage# = Temp
    n% = 1
END SUB    '{initialize}

SUB Process
    initialize
    Second% = 0
    OldSecond% = Second%
    DO
	TM$ = TIME$
	Second% = VAL(MID$(TM$, 7, 2))
	IF Second% <> OldSecond% THEN
	    n% = n% + 1
	    IF n% = 86400 THEN
		initialize  '{check 24 hours}
	    END IF
	    OldSecond% = Second%
	    ReadADchannel 1
	    Temp# = (AD%(1) - 50) / 2'{Convert data to temperature}
	    TempAverage# = TempAverage# * ((n% - 1) / n%) + Temp# / n% '{average temp}
	    IF Temp# < TempMin# THEN
		TempMin# = Temp# '{minimum temperature}
	    END IF
	    IF Temp# > TempMax# THEN
		TempMax# = Temp#   '{maximum temperature}
	    END IF
	    LOCATE 3, 7
	    PRINT MID$(TM$, 1, 8); sp$
	    LOCATE 4, 7
	    PRINT Temp#; sp$
	    LOCATE 5, 7
	    PRINT TempAverage#; sp$
	    LOCATE 6, 7
	    PRINT TempMin#; sp$
	    LOCATE 7, 7
	    PRINT TempMax#; sp$
      END IF
  it = INT(RND * 100)
  it2 = INT(RND * 100)
  LINE (pos1, 100)-(pos1, 320), 15
  LINE (pos1 + 1, 100)-(pos1 + 1, 320), 0
  LINE (0, 200)-(639, 200), 2
  PSET (pos1, it + 100)
  PSET (pos1, it2 + 205)
  pos1 = pos1 + 1
  IF pos1 > 638 THEN pos1 = 0
  LOOP UNTIL INKEY$ = CHR$(27)
END SUB    '{process}

'8 BIT AD CONVERTER SUBROUTINES
SUB ReadADchannel (ChannelNo%)
	ChipCode% = ADDAchipCode%((ChannelNo% - 1) \ 4)

	I2Cstart

	I2Coutput ChipCode%

	I2Cclockpulse

	Serdata% = 64 OR ((ChannelNo% - 1) MOD 4)
	I2Coutput Serdata%

	I2Cclockpulse

	I2Cstop

	I2Cstart

	Serdata% = ChipCode% OR 1
	I2Coutput Serdata%

	I2Cclockpulse

	AD%(ChannelNo%) = I2Cinput

	I2CmasterClockpulse

	AD%(ChannelNo%) = I2Cinput

	I2Cclockpulse

	I2Cstop
END SUB

SUB ScreenLayout
    CLS
    SCREEN 9
    COLOR 1, 15
    CLS
   
    PRINT "presse esc para sair"
    PRINT "Aplicacao : Oscilador"
    PRINT "Horas:"
    PRINT "Volts:"
    PRINT "Volts:"
    PRINT "Volts:"
    PRINT "Volts:"
    

END SUB    '{screenlayout}

' I2C COMMUNICATION SUBROUTINES
SUB SelectI2CprinterPort (PrinterNo%)
	SELECT CASE PrinterNo%
		CASE 0
			StatusPort% = 957
			ControlPort% = 958
		CASE 1
			StatusPort% = 889
			ControlPort% = 890
		CASE 2
			StatusPort% = 633
			ControlPort% = 634
	END SELECT
END SUB

FUNCTION SHL% (Dec%, Positions%)
	Temp$ = RIGHT$(DecToBin(Dec%) + STRING$(Positions%, "0"), 8)
	SHL = BinToDec(Temp$)
END FUNCTION

