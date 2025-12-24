# SSY5060V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5060V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　トステムビバ支払明細書データ（新）*
*    業務名　　　　　　　：　返品データＣＳＶ                  *
*    モジュール名　　　　：　返品データＣＳＶ出力              *
*    作成日／更新日　　　：　10/04/06                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　返品明細データをＣＳＶファイルに  *
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY5060V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/04/06.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<返品データ >>*********************************************
     SELECT   TOSSHI             ASSIGN    TO   TOSSHI
                                 STATUS         TOS-STATUS.
*
*****<<返品データCSV   >>**************************************
     SELECT   TOSCSV             ASSIGN    TO   TOSCSVH
                                 STATUS         CSV-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = トステムビバ　返品データ                           *
*--------------------------------------------------------------*
 FD  TOSSHI             BLOCK CONTAINS 1    RECORDS.
 01  TOS-REC.
     03  TOS-F01        PIC       X(01).
     03  TOS-F02        PIC       X(127).
*
*--------------------------------------------------------------*
*    FILE = トステムビバ　返品ＣＳＶデータ                     *
*--------------------------------------------------------------*
 FD  TOSCSV             BLOCK CONTAINS 1    RECORDS.
 01  MEISAI-REC.
     03  FILLER         PIC       X(410).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*返品データ格納領域
     COPY   KHJOHOF  OF XFDLIB  JOINING   KHJ  AS   PREFIX.
*ＣＳＶ情報格納領域
     COPY   TOSCSVSJ OF XFDLIB  JOINING   CSV  AS   PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  TOS-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
***** ワーク
 01  WRK-CSV.
     03  WRK-SURYO                PIC       9(05).
     03  WRK-TKUBUN               PIC       X(01).
     03  WRK-JKUBUN               PIC       X(01).

***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY5060V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5060V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY5060V".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TOSSHI.
     MOVE     "TOSSHI  "          TO        ERR-FL-ID.
     MOVE     TOS-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TOSCSV.
     MOVE     "TOSCSV"          TO        ERR-FL-ID.
     MOVE     CSV-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY5060V-START         SECTION.
*
     MOVE   "SSY5060V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY5060V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     TOSSHI.
     OPEN     OUTPUT    TOSCSV.
     DISPLAY  MSG-START UPON CONS.
*
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     PERFORM  TOSSHI-RD-SEC.
*
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    支払データ読込み
****************************************************************
 TOSSHI-RD-SEC             SECTION.
*
     MOVE    "TOSSHI-RD-SEC"    TO   S-NAME.
*
     READ     TOSSHI
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
 TOSSHI-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     EVALUATE TOS-F01
        WHEN  "B"
              PERFORM  HEAD1-EDIT-SEC
        WHEN  "C"
              PERFORM  HEAD2-EDIT-SEC
        WHEN  "D"
              PERFORM  BODY-EDIT-SEC
              PERFORM  CSVF-WRITE-SEC
              MOVE     SPACE      TO   KHJ-REC
              INITIALIZE               KHJ-REC
     END-EVALUATE.
*    次レコード読込み
     PERFORM  TOSSHI-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             ヘッダ１データ転送
****************************************************************
 HEAD1-EDIT-SEC         SECTION.
*
     MOVE    "HEAD1-EDIT-SEC"     TO   S-NAME.
*初期化
     MOVE     SPACE               TO   KHJ-HED.
     INITIALIZE                        KHJ-HED.
     MOVE     TOS-REC             TO   KHJ-HED
*項目転送
     MOVE     ","                 TO   CSV-D01 CSV-D02 CSV-D03
                                       CSV-D04 CSV-D05 CSV-D06
                                       CSV-D07 CSV-D08 CSV-D09
                                       CSV-D10 CSV-D11 CSV-D12
                                       CSV-D13 CSV-D14 CSV-D15
                                       CSV-D16.
*
     MOVE     KHJ-HD01            TO   CSV-HD01.
     MOVE     KHJ-HD02            TO   CSV-HD02.
     MOVE     KHJ-HD03            TO   CSV-HD03.
     MOVE     KHJ-HD04            TO   CSV-HD04.
     MOVE     KHJ-HD05            TO   CSV-HD05.
     MOVE     KHJ-HD06            TO   CSV-HD06.
     MOVE     KHJ-HD07            TO   CSV-HD07.
     MOVE     KHJ-HD08            TO   CSV-HD08.
     MOVE     KHJ-HD09            TO   CSV-HD09.
     MOVE     KHJ-HD10            TO   CSV-HD10.
     MOVE     KHJ-HD11            TO   CSV-HD11.
     MOVE     KHJ-HD12            TO   CSV-HD12.
     MOVE     KHJ-HD13            TO   CSV-HD13.
     MOVE     KHJ-HD14            TO   CSV-HD14.
     MOVE     KHJ-HD15            TO   CSV-HD15.
     MOVE     KHJ-HD16            TO   CSV-HD16.
*
 HEAD1-EDIT-EXIT.
     EXIT.
*
****************************************************************
*             ヘッダ２データ転送
****************************************************************
 HEAD2-EDIT-SEC         SECTION.
*
     MOVE    "HEAD2-EDIT-SEC"     TO   S-NAME.
*初期化
     MOVE     SPACE               TO   KHJ-HME.
     INITIALIZE                        KHJ-HME.
     MOVE     TOS-REC             TO   KHJ-HME
*項目転送
     MOVE     ","                 TO   CSV-M01 CSV-M02 CSV-M03
                                       CSV-M04 CSV-M05 CSV-M06
                                       CSV-M07 CSV-M08 CSV-M09
                                       CSV-M10 CSV-M11.
*
     MOVE     KHJ-HM01            TO   CSV-HM01.
     MOVE     KHJ-HM02            TO   CSV-HM02.
     MOVE     KHJ-HM03            TO   CSV-HM03.
     MOVE     KHJ-HM04            TO   CSV-HM04.
     MOVE     KHJ-HM05            TO   CSV-HM05.
     MOVE     KHJ-HM06            TO   CSV-HM06.
     MOVE     KHJ-HM07            TO   CSV-HM07.
     MOVE     KHJ-HM08            TO   CSV-HM08.
     MOVE     KHJ-HM09            TO   CSV-HM09.
     MOVE     KHJ-HM10            TO   CSV-HM10.
     MOVE     KHJ-HM11            TO   CSV-HM11.
*
 HEAD1-EDIT-EXIT.
     EXIT.
*
****************************************************************
*             明細データ転送                                   *
****************************************************************
 BODY-EDIT-SEC          SECTION.
*
     MOVE    "BODY-EDIT-SEC"      TO   S-NAME.
*初期化
     MOVE     SPACE               TO   KHJ-MEI.
     INITIALIZE                        KHJ-MEI.
     MOVE     TOS-REC             TO   KHJ-MEI
*項目転送
     MOVE     ","                 TO   CSV-E01 CSV-E02 CSV-E03
                                       CSV-E04 CSV-E05 CSV-E06
                                       CSV-E07 CSV-E08 CSV-E09
                                       CSV-E10 CSV-E11 CSV-E12
                                       CSV-E13 CSV-E14 CSV-E15
                                       CSV-E16 CSV-E17 CSV-E18
                                       CSV-E19.
*
     MOVE     KHJ-ME01            TO   CSV-ME01.
     MOVE     KHJ-ME02            TO   CSV-ME02.
     MOVE     KHJ-ME03            TO   CSV-ME03.
     MOVE     KHJ-ME04            TO   CSV-ME04.
     MOVE     KHJ-ME05            TO   CSV-ME05.
     MOVE     KHJ-ME06            TO   CSV-ME06.
     MOVE     KHJ-ME07            TO   CSV-ME07.
     MOVE     KHJ-ME08            TO   CSV-ME08.
     MOVE     KHJ-ME09            TO   CSV-ME09.
     MOVE     KHJ-ME10            TO   CSV-ME10.
     MOVE     KHJ-ME11            TO   CSV-ME11.
     MOVE     KHJ-ME12            TO   CSV-ME12.
     MOVE     KHJ-ME13            TO   CSV-ME13.
     MOVE     KHJ-ME14            TO   CSV-ME14.
     MOVE     KHJ-ME15            TO   CSV-ME15.
     MOVE     KHJ-ME16            TO   CSV-ME16.
     MOVE     KHJ-ME17            TO   CSV-ME17.
     MOVE     KHJ-ME18            TO   WRK-CSV.
     MOVE     WRK-SURYO           TO   CSV-ME18.
     MOVE     WRK-TKUBUN          TO   CSV-ME19.
     MOVE     WRK-JKUBUN          TO   CSV-ME20.
*
 BODY-EDIT-SEC-EXIT.
     EXIT.
*
****************************************************************
*             ＣＳＶファイル出力                               *
****************************************************************
 CSVF-WRITE-SEC           SECTION.
*
     MOVE    "CSVF-WRITE-SEC"     TO   S-NAME.
*初期化
     MOVE     SPACE               TO   MEISAI-REC.
*
     MOVE     CSV-REC             TO   MEISAI-REC.
     WRITE    MEISAI-REC.
     ADD      1                   TO   OUTPUT-CNT.
*
 CSVF-WRITE-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
*
     CLOSE    TOSSHI   TOSCSV.
*
 END-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************

```
