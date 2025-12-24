# SSV0010V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSV0010V.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　買掛　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　請求明細ＣＳＶデータ作成　　　　　*
*    作成日／作成者　　　：　2019/03/05 INOUE                  *
*    処理概要　　　　　　：　請求合計ファイルよりＣＳＶデータ　*
*                        ：　を作成する。　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSV0010V.
*流用                   SSE0070L SSY5130V
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/03/05.
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
     SELECT   SETGKFA2  ASSIGN         DA-01-VI-SETGKFA2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01   SEI-F03
                                       SEI-F04   SEI-F05
                        STATUS         SEI-ST.
*----<< 取引先マスタ >>--*
     SELECT   TOKMS2    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   TENMS1    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< ＣＳＶ >>-*
     SELECT   CSVGKFA   ASSIGN    TO   DA-01-S-CSVGKFA
                        ORGANIZATION   SEQUENTIAL
                        ACCESS  MODE   SEQUENTIAL
                        FILE  STATUS   CSV-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSV00101    OF        XMDLIB.
*----<< 請求合計ファイル >>--*
 FD  SETGKFA2            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< ＣＳＶ >>-*
 FD  CSVGKFA            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  20        RECORDS.
     COPY     CSVGKFA1  OF        XFDLIB
              JOINING   CSV       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*
*振分ＣＳＶ（明細）マルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     CSVGKFA1  OF        XFDLIB
              JOINING   CSV1 AS   PREFIX.
* 2.項目タイトル行1
     COPY     CSVGKFA2  OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 3.明細行
     COPY     CSVGKFA3  OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
*
 01  FLGS.
     03  END-FLG        PIC  X(03) VALUE SPACE.
*
*    金額編集
 01  WK-HEN                 PIC   9(09).
*
 01  WK-HEN1.
     03  WK-HEN1-1          PIC   X(01).
     03  WK-HEN1-2          PIC   X(09).
*
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  OUT-CNT        PIC  9(07).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  CSV-ST             PIC  X(02).
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
     DISPLAY  "### SSV0010V DSPFILE ERROR " DSP-CNTL " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"
                                       UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 請求合計ファイル >>--*
 SEI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKFA2.
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "### OSKT300 SETGKFA2 ERROR " SEI-ST " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"
                                       UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 TOKMS2-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKMS2.
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "### SSV0010V TOKMS2 ERROR " TOK-ST " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"
                                       UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 TENMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "### SSV0010V TENMS1 ERROR " TEN-ST " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"
                                       UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< ＣＳＶ >>--*
 CSVGKFA-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CSVGKFA.
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "### SSV0010V CSVGKFA ERROR " CSV-ST " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"
                                       UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
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
     DISPLAY  "*** SSV0010V START *** "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     SETGKFA2.
     OPEN     INPUT     TOKMS2.
     OPEN     INPUT     TENMS1.
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
*----<< CSV >>-*
     PERFORM  240-CSV      UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    DSPFILE.
     CLOSE    SETGKFA2.
     CLOSE    TOKMS2.
     CLOSE    TENMS1.
*
     DISPLAY  NC"入力データ＝" IN-CNT  NC"件" UPON CONS.
     DISPLAY  NC"出力ＣＳＶ＝" OUT-CNT NC"件" UPON CONS.
*
     ACCEPT   WK-DATE       FROM DATE.
     ACCEPT   WK-TIME       FROM TIME.
     DISPLAY  "*** SSV0010V END *** "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE     SPACE          TO   FSV00101.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSV00101"     TO   DSP-FMT.
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
              MOVE      4000      TO   PROGRAM-STATUS
         WHEN ENT
              IF   (    R00001    IS   NUMERIC
                   AND  R00002    IS   NUMERIC
                   AND  R00001    >    R00002    )
                        MOVE NC"範囲指定が違います"   TO   MSG
              ELSE
                        MOVE      9         TO   GR-NO
              END-IF
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
              MOVE      4000      TO   PROGRAM-STATUS
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
*    LEVEL  3      CSV                                        *
*--------------------------------------------------------------*
 240-CSV              SECTION.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     OPEN     OUTPUT    CSVGKFA.
     INITIALIZE         COUNTERS.
*
     INITIALIZE         SEI-F01.
     INITIALIZE         SEI-F03.
     INITIALIZE         SEI-F04.
     INITIALIZE         SEI-F05.
     IF       R00001    IS   NUMERIC
              MOVE      R00001    TO   SEI-F01
     END-IF.
     PERFORM  900-SEI-START-READ.
     PERFORM  241-CSV-OUT
                  UNTIL   END-FLG  =   "END".
     CLOSE    CSVGKFA.
*
     MOVE     99             TO   GR-NO.
 240-CSV-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 241-CSV-OUT         SECTION.
*
 241-CSV-01.
*  ＣＳＶ SET/OUT  1.帳票タイトル行
     PERFORM  CSV1-SET-SEC.
     WRITE    CSV-REC   FROM      CSV1-REC.
     MOVE     SPACE     TO        CSV1-REC.
     WRITE    CSV-REC   FROM      CSV1-REC.
*
*  ＣＳＶ SET/OUT  2.項目タイトル行
     PERFORM  CSV2-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*
 241-CSV-02.
*
*  ＣＳＶ SET/OUT  3.明細行
     PERFORM  CSV3-SET-SEC.
*  レコード出力
     WRITE    CSV-REC   FROM  CSV3-REC.
*  出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*  INPUT 読込み
     PERFORM  900-SEI-READ.
     IF       END-FLG    =    "END"
              GO        TO    241-CSV-OUT-EXIT
     END-IF.
*
     GO       TO       241-CSV-02.
*
 241-CSV-OUT-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ  SET   1.帳票タイトル行
****************************************************************
 CSV1-SET-SEC     SECTION.
*
 CSV1-SET-01.
*
     MOVE    SPACE                              TO CSV1-REC.
*
     MOVE    X"28"                              TO CSV1-CS00.
     MOVE    NC"【請求明細】"                   TO CSV1-C00.
     MOVE    X"29"                              TO CSV1-CE00.
     MOVE    ",,"                               TO CSV1-CK00.
*
     MOVE    X"28"                              TO CSV1-CS01.
     MOVE    NC"処理日："                       TO CSV1-C01.
     MOVE    X"29"                              TO CSV1-CE01.
     MOVE    ","                                TO CSV1-CK01.
*
     MOVE    SYS-DATE(1:4)                      TO CSV1-C02(1:4).
     MOVE    "/"                                TO CSV1-C02(5:1).
     MOVE    SYS-DATE(5:2)                      TO CSV1-C02(6:2).
     MOVE    "/"                                TO CSV1-C02(8:1).
     MOVE    SYS-DATE(7:2)                      TO CSV1-C02(9:2).
     MOVE    ","                                TO CSV1-CK02.
*
     MOVE    X"28"                              TO CSV1-CS03.
     MOVE    NC"締切日："                       TO CSV1-C03.
     MOVE    X"29"                              TO CSV1-CE03.
     MOVE    ","                                TO CSV1-CK03.
*
     MOVE    "20"                               TO CSV1-C04(1:2).
     MOVE    SEI-F02(1:2)                       TO CSV1-C04(3:2).
     MOVE    "/"                                TO CSV1-C04(5:1).
     MOVE    SEI-F02(3:2)                       TO CSV1-C04(6:2).
     MOVE    "/"                                TO CSV1-C04(8:1).
     MOVE    SEI-F02(5:2)                       TO CSV1-C04(9:2).
     MOVE    ","                                TO CSV1-CK04.
*
 CSV11-SET-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ   SET   2.項目タイトル行
****************************************************************
 CSV2-SET-SEC     SECTION.
*
 CSV2-SET-01.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-KS01.
     MOVE    NC"取引先"                           TO CSV2-K01.
     MOVE    X"29"                                TO CSV2-KE01.
     MOVE    ","                                  TO CSV2-KK01.
*
     MOVE    X"28"                                TO CSV2-KS02.
     MOVE    NC"　"                               TO CSV2-K02.
     MOVE    X"29"                                TO CSV2-KE02.
     MOVE    ","                                  TO CSV2-KK02.
*
     MOVE    X"28"                                TO CSV2-KS03.
     MOVE    NC"店舗"                             TO CSV2-K03.
     MOVE    X"29"                                TO CSV2-KE03.
     MOVE    ","                                  TO CSV2-KK03.
*
     MOVE    X"28"                                TO CSV2-KS04.
     MOVE    NC"　"                               TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"検収日"                           TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"伝票番号"                         TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
     MOVE    X"28"                                TO CSV2-KS07.
     MOVE    NC"請求金額"                         TO CSV2-K07.
     MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"備考"                             TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
     MOVE    X"28"                                TO CSV2-KS09.
     MOVE    NC"伝票区分"                         TO CSV2-K09.
     MOVE    X"29"                                TO CSV2-KE09.
     MOVE    ","                                  TO CSV2-KK09.
*
     MOVE    X"28"                                TO CSV2-KS10.
     MOVE    NC"請求区分"                         TO CSV2-K10.
     MOVE    X"29"                                TO CSV2-KE10.
     MOVE    ","                                  TO CSV2-KK10.
*
     MOVE    X"28"                                TO CSV2-KS11.
     MOVE    NC"発注日"                           TO CSV2-K11.
     MOVE    X"29"                                TO CSV2-KE11.
     MOVE    ","                                  TO CSV2-KK11.
*
     MOVE    X"28"                                TO CSV2-KS12.
     MOVE    NC"納品日"                           TO CSV2-K12.
     MOVE    X"29"                                TO CSV2-KE12.
     MOVE    ","                                  TO CSV2-KK12.
*
 CSV2-SET-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ   SET   3.明細行
****************************************************************
 CSV3-SET-SEC     SECTION.
*
     MOVE     SPACE    TO   CSV3-REC.
*
 CSV3-SET-01.
*
*制御バイト
     MOVE    X"28"     TO   CSV3-MS01
                            CSV3-MS04.
     MOVE    X"29"     TO   CSV3-ME01
                            CSV3-ME04.
*取引先ＣＤ
     MOVE     SEI-F01        TO   CSV3-M01 TOK-F01 TEN-F52.
     MOVE     ","            TO   CSV3-MK01.
*取引先名
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   CSV3-M02.
     MOVE     ","            TO   CSV3-MK02.
*店舗ＣＤ
     MOVE     SEI-F03        TO   CSV3-M03  TEN-F011.
     MOVE     ","            TO   CSV3-MK03.
*店舗名
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   CSV3-M04.
     MOVE     ","            TO   CSV3-MK04.
*検収日
     MOVE     SEI-F04(1:2)   TO   CSV3-M05(1:2).
     MOVE     "."            TO   CSV3-M05(3:1).
     MOVE     SEI-F04(3:2)   TO   CSV3-M05(4:2).
     MOVE     "."            TO   CSV3-M05(6:1).
     MOVE     SEI-F04(5:2)   TO   CSV3-M05(7:2).
     MOVE     ","            TO   CSV3-MK05.
*伝票番号
     MOVE     SEI-F05        TO   CSV3-M06.
     MOVE     ","            TO   CSV3-MK06.
*請求金額
*    MOVE     SEI-F06        TO   CSV3-M07.
     INITIALIZE                   WK-HEN1.
     IF       SEI-F06  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     SEI-F06        TO   WK-HEN.
     MOVE     WK-HEN         TO   WK-HEN1-2.
     MOVE     WK-HEN1        TO   CSV3-M07.
     MOVE     ","            TO   CSV3-MK07.
*備考
     IF       SEI-F01  =  30402
              MOVE     SEI-F12        TO   CSV3-M08
     ELSE
              IF    SEI-F01 = 87371
                OR  SEI-F01 = 87372
                OR  SEI-F01 = 87373
                OR  SEI-F01 = 87374
                OR  SEI-F01 = 87375
                OR  SEI-F01 = 33189
                OR  SEI-F01 = 30402
                OR  SEI-F01 = 304021
                  MOVE     SEI-F12        TO   CSV3-M08(7:4)
                  IF  SEI-F12 = SPACE
                      MOVE "ﾌﾞﾝﾙｲNG]]]"   TO   CSV3-M08
                  END-IF
              END-IF
     END-IF.
     MOVE     ","            TO   CSV3-MK08.
*伝票区分
     MOVE     SEI-F07        TO   CSV3-M09.
     MOVE     ","            TO   CSV3-MK09.
*請求区分
     MOVE     SEI-F08        TO   CSV3-M10.
     MOVE     ","            TO   CSV3-MK10.
*発注日
     MOVE     SEI-F13(1:4)   TO   CSV3-M11(1:4).
     MOVE     "/"            TO   CSV3-M11(5:1).
     MOVE     SEI-F13(5:2)   TO   CSV3-M11(6:2).
     MOVE     "/"            TO   CSV3-M11(8:1).
     MOVE     SEI-F13(7:2)   TO   CSV3-M11(9:2).
     MOVE     ","            TO   CSV3-MK11.
*納品日
     MOVE     SEI-F14(1:4)   TO   CSV3-M12(1:4).
     MOVE     "/"            TO   CSV3-M12(5:1).
     MOVE     SEI-F14(5:2)   TO   CSV3-M12(6:2).
     MOVE     "/"            TO   CSV3-M12(8:1).
     MOVE     SEI-F14(7:2)   TO   CSV3-M12(9:2).
     MOVE     ","            TO   CSV3-MK12.
*
 CSV3-SET-EXIT.
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
     WRITE    FSV00101.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     TOKMS2    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     TENMS1    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 START READ                *
*--------------------------------------------------------------*
 900-SEI-START-READ     SECTION.
     START    SETGKFA2   KEY  >=   SEI-F01   SEI-F03
                                  SEI-F04   SEI-F05
              INVALID   KEY
*                       MOVE HIGH-VALUE     TO   NEW
                        MOVE "END"          TO   END-FLG
     END-START.
*    IF       NEW  NOT  =    HIGH-VALUE
     IF       END-FLG  NOT  =  "END"
              PERFORM   900-SEI-READ
     END-IF.
 900-SEI-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     SETGKFA2   AT   END
*             MOVE      HIGH-VALUE     TO   NEW
              MOVE      "END"          TO   END-FLG
              GO   TO   900-SEI-READ-EXIT
     END-READ.
     IF       R00002    IS   NUMERIC
     AND      SEI-F01   >    R00002
*             MOVE      HIGH-VALUE     TO   NEW
              MOVE      "END"          TO   END-FLG
              GO   TO   900-SEI-READ-EXIT
     END-IF.
*
     IF       SEI-F08   =  9      OR
              ( ( SEI-F06        <  ZERO  ) AND
                ( SEI-F05(1:2) NOT = "07" ) AND
                ( SEI-F01         =  173  ) )
              GO   TO   900-SEI-READ
     END-IF.
*
     ADD      1              TO   IN-CNT.
*    MOVE     SEI-F03        TO   NEW-TEN.
*    MOVE     SEI-F01        TO   NEW-TOR.
 900-SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
