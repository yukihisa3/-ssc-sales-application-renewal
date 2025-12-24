# SSE7118L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE7118L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求明細書作成（Ｊ本田　新）　　　*
*    作成日／更新日　　　：　05/08/19                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより請求明細書を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*    作成日／更新日　　　：　17/04/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　店舗４桁対応　　　　　　　　　　　*
*    作成日／更新日　　　：　17/07/31                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　レーザー対応　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE7118L.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          05/08/19.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 請求合計ファイル >>--*
     SELECT   JHJSEKF   ASSIGN         DA-01-VI-JHJSEKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01   SEI-F02
                                       SEI-F03
                        STATUS         SEI-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSE71161    OF        XMDLIB.
*----<< 請求合計ファイル >>--*
 FD  JHJSEKF            LABEL RECORD   IS   STANDARD.
     COPY     JHJSEKF   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
*01  SYS-DATE           PIC  9(06).
*01  FILLER             REDEFINES      SYS-DATE.
*    03  SYS-YY         PIC  9(02).
*    03  SYS-MM         PIC  9(02).
*    03  SYS-DD         PIC  9(02).
*01  SYS-TIME           PIC  9(08).
*01  FILLER             REDEFINES      SYS-TIME.
*    03  SYS-HH         PIC  9(02).
*    03  SYS-MN         PIC  9(02).
*    03  SYS-SS         PIC  9(02).
*    03  SYS-MS         PIC  9(02).
*01  WK-DATE            PIC  9(06).
*01  FILLER             REDEFINES      WK-DATE.
*    03  WK-YY          PIC  9(02).
*    03  WK-MM          PIC  9(02).
*    03  WK-DD          PIC  9(02).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME.
         05  WK-HH                PIC  9(02)  VALUE  ZERO.
         05  WK-MN                PIC  9(02)  VALUE  ZERO.
         05  WK-SS                PIC  9(02)  VALUE  ZERO.
         05  WK-MS                PIC  9(02)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*日付項目編集
 01  KEN-DATE-AREA.
     03  KEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  KEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  KEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-TBL        OCCURS    3.
         05  KEI-KIN    PIC  S9(12)V99   PACKED-DECIMAL.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TEN              PIC  9(05).
         05  NEW-01.
             07  NEW-TOR          PIC  9(08)  VALUE 2243.
     03  OLD.
         05  OLD-TEN              PIC  9(05).
         05  OLD-01.
             07  OLD-TOR          PIC  9(08)  VALUE 2243.
 01  H-NEW.
     03  H-NEW-TENCD                  PIC  9(05).
     03  H-NEW-HIDUK                  PIC  9(08).
 01  H-OLD.
     03  H-OLD-TENCD                  PIC  9(05).
     03  H-OLD-HIDUK                  PIC  9(08).
 01  KAI-FLG                      PIC  9.

*95/08/19 START
 01  HEAD-TOR                     PIC  9(08).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了".
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"取引先".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  HD-03      PIC  9(08)  VALUE  2243.
         05  FILLER     PIC  X(08)  VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  N(09)  VALUE
                        NC"【　請求明細書　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"処理日".
         05  FILLER     PIC  X(01)  VALUE  ":".
         05  HD-011     PIC  99.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"頁".
         05  FILLER     PIC  X(01)  VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(09)  VALUE  SPACE.
         05  HD-04      PIC  N(10)  VALUE  NC"ジョイフル本田".
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(56)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"締切日：".
         05  HD-051     PIC  99.
         05  FILLER     PIC  N(01)  VALUE  NC"年".
         05  HD-052     PIC  Z9.
         05  FILLER     PIC  N(01)  VALUE  NC"月".
         05  HD-053     PIC  Z9.
         05  FILLER     PIC  N(01)  VALUE  NC"日".
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
*#31     05  FILLER     PIC  X(04)  VALUE  SPACE.
*#31     05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"店　舗".
*********05  FILLER     PIC  X(22)  VALUE  SPACE.
         05  FILLER     PIC  X(21)  VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(06)  VALUE  NC"検収年月日".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"伝票_".
         05  FILLER     PIC  X(08)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"伝票区分".
         05  FILLER     PIC  X(08)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"請求金額".
         05  FILLER     PIC  X(04)  VALUE  SPACE.
*********05  FILLER     PIC  N(03)  VALUE  NC"備　考".
         05  FILLER     PIC  N(01)  VALUE  NC"部".
         05  FILLER     PIC  X(03)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"項".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
*****03  FILLER         CHARACTER TYPE MODE-2.
*#31     05  FILLER     PIC  X(04).
*#31     05  FILLER     PIC  X(02).
         05  ME-06      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-07      PIC  N(10).
         05  FILLER     PIC  X(03).
         05  ME-081     PIC  9999.
         05  ME-081P    PIC  X(01).
         05  ME-082     PIC  Z9.
         05  ME-082P    PIC  X(01).
         05  ME-083     PIC  Z9.
         05  FILLER     PIC  X(03).
         05  ME-09      PIC  X(09).
         05  FILLER     PIC  X(03).
         05  ME-091     PIC  99.
         05  FILLER     PIC  X(01).
         05  ME-092     PIC  N(02).
         05  FILLER     PIC  X(01).
         05  ME-10      PIC  ----,---,--9.
         05  FILLER     PIC  X(03).
         05  ME-101     PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-102     PIC  X(02).
 01  MEIS02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(33)  VALUE  SPACE.
         05  FILLER     PIC  N(02)  VALUE  NC"＊＊".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"店".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"舗".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"計".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(02)  VALUE  NC"＊＊".
         05  FILLER     PIC  X(10)  VALUE  SPACE.
         05  ME-11      PIC  ----,---,--9.
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(33)  VALUE  SPACE.
         05  FILLER     PIC  N(02)  VALUE  NC"＊＊".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"ページ計".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(02)  VALUE  NC"＊＊".
         05  FILLER     PIC  X(11)  VALUE  SPACE.
         05  ME-12      PIC  ----,---,--9.
 01  MEIS04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(33)  VALUE  SPACE.
         05  FILLER     PIC  N(02)  VALUE  NC"＊＊".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"合".
         05  FILLER     PIC  X(04)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"計".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(02)  VALUE  NC"＊＊".
         05  FILLER     PIC  X(08)  VALUE  SPACE.
         05  ME-13      PIC  --,---,---,--9.
 01  P-SPACE            PIC  X(01)  VALUE  SPACE.
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "### SSE0070L DSPFILE ERROR " DSP-CNTL " "
     SYS-DATE(1:4) "." SYS-DATE(5:2) "." SYS-DATE(7:2) " "
              WK-HH ":" WK-MN ":" WK-SS " ###"
                                       UPON CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*----<< 請求合計ファイル >>--*
 SEI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JHJSEKF.
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "### OSKT300 JHJSEKF ERROR " SEI-ST " "
     SYS-DATE(1:4) "." SYS-DATE(5:2) "." SYS-DATE(7:2) " "
              WK-HH ":" WK-MN ":" WK-SS " ###"
                                       UPON CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "### SSE0070L HTENMS ERROR " TEN-ST " "
     SYS-DATE(1:4) "." SYS-DATE(5:2) "." SYS-DATE(7:2) " "
              WK-HH ":" WK-MN ":" WK-SS " ###"
                                       UPON CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
***
     DISPLAY  "*** SSE7118L START *** "
     SYS-DATE(1:4) "." SYS-DATE(5:2) "." SYS-DATE(7:2) " "
              WK-HH ":" WK-MN ":" WK-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     JHJSEKF.
     OPEN     INPUT     HTENMS.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ｲﾝｻﾂ >>-*
     PERFORM  240-PRINT      UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    DSPFILE.
     CLOSE    JHJSEKF.
     CLOSE    HTENMS.
*
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "*** SSE7106L END   *** "
     SYS-DATE(1:4) "." SYS-DATE(5:2) "." SYS-DATE(7:2) " "
              WK-HH ":" WK-MN ":" WK-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE     SPACE          TO   FSE71161.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSE71161"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "GRP01"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              MOVE      9         TO   GR-NO
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN ENT
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 240-PRINT              SECTION.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     OPEN     OUTPUT    PRTF.
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE                   GOKEI-AREA.
*
     MOVE     ZERO           TO   SEI-F01.
     INITIALIZE         SEI-F02.
     INITIALIZE         SEI-F03.
     INITIALIZE         SEI-F04.
     INITIALIZE         SEI-F05.
     PERFORM  900-SEI-START-READ.
     PERFORM  241-LIST-PRINT
                        UNTIL     OLD  =    HIGH-VALUE.
     CLOSE    PRTF.
*
     MOVE     0              TO   GR-NO.
 240-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 241-LIST-PRINT         SECTION.
     IF       OLD       NOT  =    LOW-VALUE
     AND      NEW       NOT  =    OLD
              PERFORM   220-MEIS02-PRINT
     END-IF.
     IF       OLD       NOT  =    LOW-VALUE
     AND      NEW-01    NOT  =    OLD-01
              PERFORM   240-MEIS04-PRINT
     END-IF.
     IF       NEW       NOT  =    HIGH-VALUE
              PERFORM   210-MEIS01-PRINT
     END-IF.
*
     MOVE     NEW            TO   OLD.
     MOVE     H-NEW          TO   H-OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   250-SYUKEI
                        VARYING   I    FROM 1    BY   1
                        UNTIL     I    >    3
              PERFORM   900-SEI-READ
     END-IF.
 241-LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 210-MEIS01-PRINT       SECTION.
*95/08/19
     IF       LINE-CNT  >    63
     OR       NEW-01    NOT  =    OLD-01
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     IF       H-NEW-TENCD    NOT =  H-OLD-TENCD
     OR       KAI-FLG        =   1
              MOVE     SEI-F02        TO   ME-06
              MOVE     2243           TO   TEN-F52
              MOVE     SEI-F02        TO   TEN-F011
              PERFORM  900-TEN-READ
              IF       TEN-F03   =   SPACE
                       IF   SEI-F02 NOT = ZERO
                            MOVE ALL NC"＊"     TO   ME-07
                       END-IF
              ELSE
                       MOVE      TEN-F03        TO   ME-07
              END-IF
              MOVE     SEI-F04        TO   KEN-DATE-AREA
              MOVE     KEN-DATE-YYYY  TO   ME-081
              MOVE     "."            TO   ME-081P
              MOVE     KEN-DATE-MM    TO   ME-082
              MOVE     "."            TO   ME-082P
              MOVE     KEN-DATE-DD    TO   ME-083
     END-IF.
     IF       H-NEW-HIDUK    NOT =    H-OLD-HIDUK

              MOVE     SEI-F04        TO   KEN-DATE-AREA
              MOVE     KEN-DATE-YYYY  TO   ME-081
              MOVE     "."            TO   ME-081P
              MOVE     KEN-DATE-MM    TO   ME-082
              MOVE     "."            TO   ME-082P
              MOVE     KEN-DATE-DD    TO   ME-083
     END-IF
     IF       SEI-F07        =    "31"
              MOVE           "31"        TO   ME-091
              MOVE           NC"仕入"    TO   ME-092
     END-IF.
     IF       SEI-F07        =    "21"
              MOVE           "21"        TO   ME-091
              MOVE           NC"返品"    TO   ME-092
              MULTIPLY       -1          BY   SEI-F10
     END-IF.
     IF       SEI-F07        =    "11"
              MOVE           "11"        TO   ME-091
              MOVE           NC"値引"    TO   ME-092
              MULTIPLY       -1          BY   SEI-F10
     END-IF.
     MOVE     SEI-F03        TO   ME-09.
     MOVE     SEI-F10        TO   ME-10.
     MOVE     SEI-F05        TO   ME-101.
     MOVE     SEI-F06        TO   ME-102.
     MOVE     ZERO           TO   KAI-FLG.
*
     IF       NEW       NOT  =    OLD
     AND      LINE-CNT  NOT  =    9
              WRITE     PRT-REC   FROM MEIS01    AFTER     2
              ADD       2         TO   LINE-CNT
     ELSE
              WRITE     PRT-REC   FROM MEIS01    AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 220-MEIS02-PRINT       SECTION.
     IF       LINE-CNT  >    61
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     KEI-KIN (01)   TO   ME-11.
     WRITE    PRT-REC        FROM MEIS02    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (01).
 220-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾍﾟｰｼﾞｹｲ ｲﾝｻﾂ                                 *
*--------------------------------------------------------------*
 230-MEIS03-PRINT       SECTION.
     MOVE     KEI-KIN (02)   TO   ME-12.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (02).
 230-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 240-MEIS04-PRINT       SECTION.
     PERFORM  230-MEIS03-PRINT.
     IF       LINE-CNT  >    61
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     KEI-KIN (03)   TO   ME-13.
     WRITE    PRT-REC        FROM MEIS04    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (03).
 240-MEIS04-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 250-SYUKEI             SECTION.
     ADD      SEI-F10        TO   KEI-KIN (I).
 250-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 211-HEAD-PRINT         SECTION.
     IF       OLD       NOT  =    LOW-VALUE
     AND      NEW-01         =    OLD-01
              PERFORM   230-MEIS03-PRINT
     END-IF.
*
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1              TO   PAGE-CNT.
     MOVE     1              TO   KAI-FLG.
     MOVE     HEN-DATE-YYYY(3:2)  TO   HD-011.
     MOVE     HEN-DATE-MM         TO   HD-012.
     MOVE     HEN-DATE-DD         TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-02.
****
****
****     IF       NEW            =    HIGH-VALUE
****              MOVE     OLD-TOR        TO   HD-03
****              MOVE     OLD-TOR        TO   TOK-F01
****     ELSE
****              MOVE     SEI-F01        TO   HD-03
****              MOVE     SEI-F01        TO   TOK-F01
****     END-IF.
****
****     PERFORM  900-TOK-READ.
     MOVE     NC"ジョイフル本田"  TO   HD-04.
     MOVE     SEI-F01(3:2)   TO   HD-051.
     MOVE     SEI-F01(5:2)   TO   HD-052.
     MOVE     SEI-F01(7:2)   TO   HD-053.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     3.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     9              TO   LINE-CNT.
 211-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "SCRERE"       TO   DSP-GRP.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSE71161.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-SEI-START-READ     SECTION.
     START    JHJSEKF   KEY  >=   SEI-F01   SEI-F02
                                  SEI-F03
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-SEI-READ
     END-IF.
 900-SEI-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     JHJSEKF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-SEI-READ-EXIT
     END-READ.
*
***  IF       SEI-F08   =    9
***  OR  (    SEI-F10   <    ZERO )
***           GO   TO   900-SEI-READ
***  END-IF.
     MOVE     SEI-F02        TO   NEW-TEN
                                  H-NEW-TENCD.
     MOVE     2243           TO   NEW-TOR.
     MOVE     SEI-F04        TO   H-NEW-HIDUK.
 900-SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
