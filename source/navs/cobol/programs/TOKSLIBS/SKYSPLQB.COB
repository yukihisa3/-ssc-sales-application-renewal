****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　スプール情報取得サブ              *
*    モジュール名　　　　：　スプール情報取得サブ              *
*    作成日／作成者　　　：　08/08/18  T.TAKAHASHI             *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　スプールキュー内の情報を取得して　*
*                            スプール情報ファイルに登録する。  *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKYSPLQB.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/08/05.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*スプール情報ファイル
     SELECT     SPOOLPF    ASSIGN    TO        SPOOLL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SPL-F01
                                               SPL-F02
                                               SPL-F03
                           FILE      STATUS    SPL-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
******************************************************************
*  FILE=条件ファイル　　                                       *
****************************************************************
 FD  SPOOLPF
     LABEL       RECORD    IS        STANDARD.
     COPY        SPOOLPF   OF        XFDLIB
     JOINING     SPL       AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  SPL-ST              PIC X(02).
***  添字
 01  IX                      PIC 9(01)  VALUE  ZERO.
***  フラグ
 01  SPOOLPF-INV-FLG         PIC X(03)  VALUE  SPACE.
***  ｻﾌﾞﾙｰﾁﾝﾊﾟﾗﾒﾀｴﾘｱ
 01  SSI-CTL-PARA.
     03  SSI-ID              PIC 9(02).
     03  FILLER              PIC X(02)  VALUE  LOW-VALUE.
     03  SSI-FUNC            PIC 9(01).
     03  SSI-SORT            PIC 9(01).
     03  FILLER              PIC X(01)  VALUE  LOW-VALUE.
     03  FILLER              PIC X(01)  VALUE  LOW-VALUE.
     03  SSI-RTNCD           PIC 9(02).
     03  SSI-PRN             PIC 9(03)  BINARY.
     03  SSI-ARN             PIC 9(03)  BINARY.
     03  SSI-RRN             PIC 9(04)  BINARY.
     03  FILLER              PIC X(04)  VALUE  LOW-VALUE.
***
 01  SSI-TIME-PARA.
     03  SSI-ERN             PIC 9(04)  BINARY.
     03  SSI-YEAR            PIC 9(04).
     03  SSI-MONTH           PIC 9(02).
     03  SSI-DAY             PIC 9(02).
     03  SSI-HOUR            PIC 9(02).
     03  SSI-MINUTE          PIC 9(02).
     03  SSI-SECOND          PIC 9(02).
*
 01  SSI-SPL-PARA.
     03  SSI-SPL-DATA        OCCURS  3  TIMES.
         05  SSI-SQ          PIC X(08).
         05  SSI-SFN         PIC X(08).
         05  SSI-SID         PIC 9(03)  BINARY.
         05  FILLER          PIC X(02).
         05  SSI-JOB         PIC X(08).
         05  SSI-PROF        PIC X(08).
         05  SSI-SFYEAR      PIC 9(04).
         05  SSI-SFMONTH     PIC 9(02).
         05  SSI-SFDAY       PIC 9(02).
         05  SSI-SFHOUR      PIC 9(02).
         05  SSI-SFMINUTE    PIC 9(02).
         05  SSI-SFSECOND    PIC 9(02).
         05  SSI-OUTPRI      PIC 9(01).
         05  SSI-SFS         PIC X(01).
         05  SSI-FORM        PIC X(08).
         05  SSI-TP          PIC 9(05)  BINARY.
*
 01  SSI-SPL-RETREAV.
     03  SSI-RSQ             PIC X(08)  VALUE  SPACE.
     03  FILLER              PIC X(12)  VALUE  LOW-VALUE.
     03  SSI-RSJOB           PIC X(08)  VALUE  SPACE.
     03  SSI-RSPRF           PIC X(08)  VALUE  SPACE.
     03  FILLER              PIC X(28)  VALUE  LOW-VALUE.
*
***
 01  FILE-ERR.
     03  SPL-ERR             PIC N(15) VALUE
                             NC"スプール情報ファイル異常".
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION.
 DECLARATIVES.
 SPL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SPOOLPF.
     DISPLAY     SPL-ERR   UPON      CONS.
     DISPLAY     SPL-ST    UPON      CONS.
     DISPLAY "SPL-F01 = " SPL-F01 UPON CONS.
     DISPLAY "SPL-F02 = " SPL-F02 UPON CONS.
     DISPLAY "SPL-F03 = " SPL-F03 UPON CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E                  *
****************************************************************
 MAIN-SEC                  SECTION.
*
     OPEN   I-O   SPOOLPF.
*
     MOVE        07               TO   SSI-ID.
     MOVE        1                TO   SSI-FUNC.
     CALL "XSBSSI" USING SSI-CTL-PARA SSI-TIME-PARA
                         SSI-SPL-RETREAV.
     IF    SSI-RTNCD  =  00
           GO            TO       SSI-GET
     ELSE
           GO            TO       EPILOGUE
     END-IF.
*
 SSI-GET.
     MOVE         2               TO    SSI-FUNC.
     MOVE         3               TO    SSI-PRN.
     CALL "XSBSSI" USING SSI-CTL-PARA SSI-SPL-PARA.
     IF    SSI-RTNCD  =  00
     OR    SSI-RTNCD  =  04
           GO                     TO    EDIT-DATA
     ELSE
           GO                     TO    SSI-CLOSE
     END-IF.
*
 EDIT-DATA.
     PERFORM  VARYING IX FROM 1 BY 1 UNTIL  IX  >  3
        IF  SSI-SQ(IX)  =  "XXLOGQ  "
            MOVE     SPACE               TO    SPL-REC
            INITIALIZE                         SPL-REC
            MOVE     SSI-SQ(IX)          TO    SPL-F01
            MOVE     SSI-SFN(IX)         TO    SPL-F02
            MOVE     SSI-SID(IX)         TO    SPL-F03
            READ     SPOOLPF
                     INVALID     MOVE  "INV"   TO  SPOOLPF-INV-FLG
                     NOT INVALID MOVE  SPACE   TO  SPOOLPF-INV-FLG
            END-READ
************DISPLAY "SPOOLF-INV-FLG = " SPOOLPF-INV-FLG UPON CONS
            IF       SPOOLPF-INV-FLG = "INV"
                     MOVE     SPACE               TO    SPL-REC
                     INITIALIZE                         SPL-REC
                     MOVE     SSI-SQ(IX)          TO    SPL-F01
                     MOVE     SSI-SFN(IX)         TO    SPL-F02
                     MOVE     SSI-SID(IX)         TO    SPL-F03
                     MOVE     SSI-JOB(IX)         TO    SPL-F04
                     MOVE     SSI-PROF(IX)        TO    SPL-F05
                     WRITE    SPL-REC
            END-IF
        END-IF
     END-PERFORM.
*
     IF   SSI-RTNCD = 04  GO      TO    SSI-GET.
*
 SSI-CLOSE.
     MOVE         9               TO    SSI-FUNC.
     CALL "XSBSSI" USING SSI-CTL-PARA.
 EPILOGUE.
     CLOSE     SPOOLPF.
     STOP  RUN.
 MAIN-EXIT.
     EXIT.
