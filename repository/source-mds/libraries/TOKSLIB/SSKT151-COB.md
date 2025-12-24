# SSKT151

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSKT151.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　伝票番号付番入力　Ｖ０２　　　　　　　*
*    作成日　　　　　：　93/06/23                              *
*    作成者　　　　　：　T.A.                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            OSKT151.
 AUTHOR.                T.T.
 DATE-WRITTEN.          93/06/23.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST1.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-ST1.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*伝票データ
     SELECT   HDENJNL   ASSIGN    TO        DA-01-VI-DENJNL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       DEN-F09   DEN-F01
                                            DEN-F02   DEN-F051
                                            DEN-F03   DEN-F04
                        FILE      STATUS    DEN-ST1.
*伝票データ（チェック用）
     SELECT   HDENJNL1  ASSIGN    TO        DA-01-VI-DENJNL15
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       DEN1-F01   DEN1-F23
                                            DEN1-F04   DEN1-F051
                                            DEN1-F03
                        FILE      STATUS    DEN1-ST1.
*画面ファイル
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        SYMBOLIC  DESTINATION        "DSP"
*******************     DESTINATION-1       DSP-WS
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING  MODE    DSP-PRO
*******************     UNIT      CONTROL   DSP-UNIT
                        SELECTED  FUNCTION  DSP-FNC
                        FILE      STATUS    DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*取引先マスタ
 FD  HTOKMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTOKMS    OF   XFDLIB
     JOINING  TOK       AS   PREFIX.
*店舗マスタ
 FD  HTENMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTENMS    OF   XFDLIB
     JOINING  TEN       AS   PREFIX.
*条件ファイル
 FD  HJYOKEN            BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HJYOKEN   OF   XFDLIB
     JOINING  JYO       AS   PREFIX.
*伝票データ
 FD  HDENJNL            BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     SHTDENF   OF   XFDLIB
     JOINING  DEN       AS   PREFIX.
*伝票データ（チェック用）
 FD  HDENJNL1           BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     SHTDENF   OF   XFDLIB
     JOINING  DEN1      AS   PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY     FSK150    OF   XMDLIB
     JOINING  DSP       AS   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "OSKT151".
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT             PIC  X(08).
     03  DSP-GRP             PIC  X(08).
     03  DSP-GRPR  REDEFINES DSP-GRP.
         05  DSP-GRPX        PIC  X(04).
         05  DSP-GRPY        PIC  9(02).
         05  DSP-GRPZ        PIC  X(02).
     03  DSP-PRO             PIC  X(02).
     03  DSP-FNC             PIC  X(04).
     03  DSP-ST1             PIC  X(02).
     03  DSP-ST2             PIC  X(04).
     03  DSP-CON             PIC  X(06).
 01  STATUS-AREA.
     03  IN-DATA             PIC  X(01).
     03  TOK-STATUS.
         05  TOK-ST1         PIC  X(02).
         05  TOK-ST2         PIC  X(04).
     03  TEN-STATUS.
         05  TEN-ST1         PIC  X(02).
         05  TEN-ST2         PIC  X(04).
     03  JYO-STATUS.
         05  JYO-ST1         PIC  X(02).
         05  JYO-ST2         PIC  X(04).
     03  DEN-STATUS.
         05  DEN-ST1         PIC  X(02).
         05  DEN-ST2         PIC  X(04).
     03  DEN1-STATUS.
         05  DEN1-ST1        PIC  X(02).
         05  DEN1-ST2        PIC  X(04).
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消　_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消　_項目戻り".
     03  G003.
         05  FILLER          PIC  N(19)  VALUE
             NC"_取消_明細終了_自社_へ_前行_次行".
         05  FILLER          PIC  N(11)  VALUE
             NC"_前頁_次頁_自動採番".
     03  G004                PIC  N(30)  VALUE
         NC"_取消　_項目戻り　_前頁　_次頁　_先頭行へ".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
                             NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
                             NC"処理区分が違います".
     03  MSG03               PIC  N(20)  VALUE
                             NC"伝発場所未登録".
     03  MSG04               PIC  N(20)  VALUE
                             NC"取引先マスタ未登録".
     03  MSG05               PIC  N(20)  VALUE
                             NC"店舗マスタ未登録".
     03  MSG06               PIC  N(20)  VALUE
                             NC"チェックデジットが違います．".
     03  MSG07               PIC  N(20)  VALUE
                             NC"伝票が未入力です".
     03  MSG08               PIC  N(20)  VALUE
                             NC"Ｙ，空白を入力".
     03  MSG09               PIC  N(20)  VALUE
                             NC"画面ダブリエラーです".
     03  MSG10               PIC  N(20)  VALUE
                             NC"既に不番済み_があります".
     03  MSG11               PIC  N(20)  VALUE
                             NC"１行目は自動採番出来ません".
     03  MSG12               PIC  N(20)  VALUE
                             NC"直前の指定_を入力して下さい".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS      12.
*01  SYS-DATE                PIC  9(06).
*01  FILLER                  REDEFINES   SYS-DATE.
*    03  SYS-YY              PIC  9(02).
*    03  SYS-MM              PIC  9(02).
*    03  WK-D              PIC  9(02).
*01  SYS-TIME.
*    03  SYS-HH              PIC  9(02).
*    03  SYS-MN              PIC  9(02).
*    03  SYS-SS              PIC  9(02).
*    03  FILLER              PIC  9(02).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME.
         05  WK-HH                PIC  9(02)  VALUE  ZERO.
         05  WK-MN                PIC  9(02)  VALUE  ZERO.
         05  WK-SS                PIC  9(02)  VALUE  ZERO.
         05  FILLER               PIC  9(02)  VALUE  ZERO.
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
 01  INDEXES.
     03  I                   PIC  9(04).
     03  J                   PIC  9(04).
 01  DSP-WORK.
     03  WK-SHORKB           PIC  9(01)    VALUE ZERO.
     03  WK-AMARI            PIC  9(02)    VALUE ZERO.
     03  WK-F02              PIC  9(09)    VALUE ZERO.
     03  ERR-SW              PIC  9(01)    VALUE ZERO.
 01  MEISAI-WORK.
     03  MEI-TBL     OCCURS  1000.
       04  MEI-TBLA.
         05  MEI-JISHNOX     PIC  X(05).
         05  MEI-JISHNO      PIC  9(09).
         05  MEI-SITENOX     PIC  X(05).
         05  MEI-SITENO      PIC  9(09).
         05  MEI-TENCDX      PIC  X(05).
         05  MEI-TENCD       PIC  9(05).
         05  MEI-TENNMX      PIC  X(05).
         05  MEI-TENNM       PIC  N(10).
       04  MEI-TBLB.
         05  MEI-SAVNO       PIC  9(09).
         05  MEI-ERRFLG      PIC  9(01).
 01  FLAGS.
     03  EOF-FLG             PIC  9(01).
     03  ERR-FLG             PIC  9(02).
     03  SHORI-FLG               PIC  9(02).
     03  DSP-OPEN-FLG        PIC  9(01).
     03  TOK-OPEN-FLG        PIC  9(01).
     03  TEN-OPEN-FLG        PIC  9(01).
     03  JYO-OPEN-FLG        PIC  9(01).
     03  DEN-OPEN-FLG        PIC  9(01).
     03  DEN1-OPEN-FLG       PIC  9(01).
 01  COUNTERS.
     03  LCNT                PIC  9(02).
     03  MCNT                PIC  9(04).
     03  PCNT                PIC  9(02).
     03  MAX-MCNT            PIC  9(04).
     03  MAX-PCNT            PIC  9(02).
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-KBN          PIC  9(01).
         05  LI-KETA         PIC  9(01).
         05  LI-START        PIC  9(09).
         05  LI-END          PIC  9(09).
         05  LI-DENNO        PIC  9(09).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-NEXT         PIC  9(09).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 PROCEDURE                 DIVISION.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  取引先マスタ　------------------------------------*
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTOKMS.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"取引先マスタ異常！"
              "ST1=" TOK-ST1                 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       DEN-OPEN-FLG  =  1  CLOSE    HDENJNL.
     IF       DEN1-OPEN-FLG =  1  CLOSE    HDENJNL1.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  店舗マスタ　--------------------------------------*
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTENMS.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"店舗マスタ異常！"
              "ST1=" TEN-ST1                 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       DEN-OPEN-FLG  =  1  CLOSE    HDENJNL.
     IF       DEN1-OPEN-FLG =  1  CLOSE    HDENJNL1.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   条件ファイル　-----------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1                 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       DEN-OPEN-FLG  =  1  CLOSE    HDENJNL.
     IF       DEN1-OPEN-FLG =  1  CLOSE    HDENJNL1.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    伝票データＦ -----------------------------------*
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HDENJNL.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"伝票データＦ異常！"
              "ST1=" DEN-ST1                 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       DEN-OPEN-FLG  =  1  CLOSE    HDENJNL.
     IF       DEN1-OPEN-FLG =  1  CLOSE    HDENJNL1.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    伝票データＦ -----------------------------------*
 DEN1-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HDENJNL1.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"伝票データ１Ｆ異常！"
              "ST1=" DEN1-ST1                " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       DEN-OPEN-FLG  =  1  CLOSE    HDENJNL.
     IF       DEN1-OPEN-FLG =  1  CLOSE    HDENJNL1.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    画面データＦ -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       DEN-OPEN-FLG  =  1  CLOSE    HDENJNL.
     IF       DEN1-OPEN-FLG =  1  CLOSE    HDENJNL1.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL     SHORI-FLG    =    99.
     PERFORM  END-SEC.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-SEC           SECTION.
*    ACCEPT      WK-DATE    FROM  DATE.
*    ACCEPT      WK-TIME    FROM  TIME.
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
*
     DISPLAY  "*** " PGM-ID " START "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ***"  UPON STAT.
*
     MOVE        ZERO      TO        FLAGS.
*
     OPEN     I-O       HTOKMS.
     MOVE     1              TO   TOK-OPEN-FLG.
     OPEN     INPUT     HTENMS.
     MOVE     1              TO   TEN-OPEN-FLG.
     OPEN     INPUT     HJYOKEN.
     MOVE     1              TO   JYO-OPEN-FLG.
     OPEN     I-O       HDENJNL.
     MOVE     1              TO   DEN-OPEN-FLG.
     OPEN     INPUT     HDENJNL1.
     MOVE     1              TO   DEN1-OPEN-FLG.
     OPEN     I-O       DSPFILE.
     MOVE     1              TO   DSP-OPEN-FLG.
*
     MOVE     0              TO   SHORI-FLG.
 INIT-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-SEC           SECTION.
     EVALUATE    SHORI-FLG
* 画面初期表示
         WHEN    0           PERFORM     DSP-INIT-SEC
* 処理区分入力
         WHEN    1           PERFORM     SHORKB-SEC
* 伝発場所入力
         WHEN    2           PERFORM     BASHKB-SEC
* 取引先Ｃ入力
         WHEN    3           PERFORM     TORICD-SEC
* 自社伝票_入力
         WHEN    4           PERFORM     JISHNO-SEC
* 指定伝票_入力
         WHEN    5           PERFORM     SITENO-SEC
* 確認入力
         WHEN    90          PERFORM     KAKU-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-SEC            SECTION.
     CLOSE              HTOKMS.
     CLOSE              HTENMS.
     CLOSE              HJYOKEN.
     CLOSE              HDENJNL.
     CLOSE              HDENJNL1.
     CLOSE              DSPFILE.
*
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ***"  UPON STAT.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*  0               ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 DSP-INIT-SEC           SECTION.
     MOVE     SPACE          TO   DSP-FSK150.
**** MOVE     SYS-DATE       TO   DSP-SYSDT.
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSK150"       TO   DSP-FMT.
     MOVE     HEN-DATE       TO   DSP-SDATE.
     MOVE     HEN-TIME       TO   DSP-STIME.
     IF   WK-SHORKB NOT = ZERO
          MOVE      WK-SHORKB TO   DSP-SHORKB
     END-IF.
     PERFORM  VARYING  I  FROM  1  BY  1   UNTIL I  >  15
        MOVE  "M"      TO EDIT-OPTION  OF  DSP-SITE01(I)
     END-PERFORM.
*
     MOVE     1              TO   SHORI-FLG.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*  1               処理区分　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 SHORKB-SEC           SECTION.
     MOVE     G001           TO   DSP-GUIDE.
     PERFORM  DSP-WT-SEC.
     MOVE     "SHORKB"       TO   DSP-GRP.
     PERFORM  DSP-RD-SEC.
*
     EVALUATE DSP-FNC
       WHEN   "F004"
                   MOVE      0         TO   SHORI-FLG
       WHEN   "F005"
                   MOVE      99        TO   SHORI-FLG
       WHEN   "E000"
                   PERFORM   CHK-SHORKB-SEC
                   IF  ERR-FLG  =  ZERO
                       MOVE     2      TO   SHORI-FLG
                   END-IF
       WHEN   OTHER
                   MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 SHORKB-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    1             処理区分　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-SHORKB-SEC           SECTION.
     IF       DSP-SHORKB  IS  NOT  NUMERIC
              MOVE      ZERO      TO   DSP-SHORKB
     END-IF.
     IF       DSP-SHORKB  NOT  =  1  AND  2
              MOVE      2         TO   ERR-FLG
     END-IF.
 CHK-SHORKB-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    2             伝発場所　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 BASHKB-SEC           SECTION.
     MOVE     G002           TO   DSP-GUIDE.
     PERFORM  DSP-WT-SEC.
     MOVE     "BASHKB"       TO   DSP-GRP.
     PERFORM  DSP-RD-SEC.
*
     EVALUATE DSP-FNC
       WHEN   "F004"
                   MOVE      0         TO   SHORI-FLG
       WHEN   "F006"
                   MOVE      1         TO   SHORI-FLG
       WHEN   "E000"
                   PERFORM   CHK-BASHKB-SEC
                   IF  ERR-FLG  =  ZERO
                       MOVE     3      TO   SHORI-FLG
                   END-IF
       WHEN   OTHER
                   MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 BASHKB-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    2             伝発場所　チェック
*--------------------------------------------------------------*
 CHK-BASHKB-SEC           SECTION.
*条件Ｆ存在ＣＨＫ
     MOVE     "20"           TO   JYO-F01.
     MOVE     DSP-BASHKB     TO   JYO-F02.
     READ     HJYOKEN
       INVALID   KEY
              MOVE     SPACE      TO   DSP-BASHNM
              MOVE     3          TO   ERR-FLG
       NOT INVALID   KEY
              MOVE     JYO-F03    TO   DSP-BASHNM
     END-READ.
 CHK-BASHKB-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    3             取引先Ｃ　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 TORICD-SEC           SECTION.
     MOVE     G002           TO   DSP-GUIDE.
     PERFORM  DSP-WT-SEC.
     MOVE     "TORICD"       TO   DSP-GRP.
     PERFORM  DSP-RD-SEC.
*
     EVALUATE DSP-FNC
       WHEN   "F004"
                   MOVE      0         TO   SHORI-FLG
       WHEN   "F006"
                   MOVE      2         TO   SHORI-FLG
       WHEN   "E000"
                   PERFORM   CHK-TORICD-SEC
                   IF  ERR-FLG  =  ZERO
                       MOVE     4      TO   SHORI-FLG
                   END-IF
       WHEN    OTHER
                   MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 TORICD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    3             取引先Ｃ　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-TORICD-SEC           SECTION.
     IF       DSP-TORICD  NOT  NUMERIC
              MOVE     ZERO       TO   DSP-TORICD
     END-IF.
*得意先Ｍ存在ＣＨＫ
     MOVE     DSP-TORICD          TO   TOK-F01.
     READ     HTOKMS
       INVALID   KEY
              MOVE     SPACE      TO   DSP-TORINM
              MOVE     4          TO   ERR-FLG
       NOT INVALID KEY
              MOVE     TOK-F03    TO   DSP-TORINM
              REWRITE  TOK-REC
     END-READ.
 CHK-TORICD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    4             自社伝票_　入力　（１行目）
*--------------------------------------------------------------*
 JISHNO-SEC           SECTION.
     MOVE     G002           TO   DSP-GUIDE.
     PERFORM  DSP-WT-SEC.
     MOVE     "JISH01"       TO   DSP-GRP.
     PERFORM  DSP-RD-SEC.
*
     EVALUATE DSP-FNC
       WHEN   "F004"
                   MOVE      0         TO   SHORI-FLG
       WHEN   "F006"
                   MOVE      3         TO   SHORI-FLG
       WHEN   "E000"
                   PERFORM   CHK-JISHNO-SEC
                   IF  ERR-FLG  =  ZERO
                       MOVE     1      TO   LCNT  MCNT PCNT
                       MOVE     5      TO   SHORI-FLG
                   END-IF
       WHEN    OTHER
                   MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 JISHNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    4             自社伝票_　チェック
*--------------------------------------------------------------*
 CHK-JISHNO-SEC           SECTION.
     IF       DSP-JISH01(1)  IS  NOT  NUMERIC
              MOVE      ZERO      TO   DSP-JISH01(1)
     END-IF.
*チェックデジットＣＨＫ
     IF       DSP-JISH01(1)  NOT  =  ZERO
              MOVE      TOK-F93        TO   LI-KBN
              MOVE      TOK-F92        TO   LI-KETA
              MOVE      TOK-F57        TO   LI-START
              MOVE      TOK-F58        TO   LI-END
              MOVE      DSP-JISH01 (1) TO   LI-DENNO
              CALL      "OSKTCDCK"     USING     LINK-AREA
              IF   LO-ERR  NOT  =  0
                   MOVE      6         TO   ERR-FLG
                   GO   TO   CHK-JISHNO-EXIT
              END-IF
     END-IF.
*伝票Ｄ存在ＣＨＫ
     MOVE     ZERO               TO   EOF-FLG.
     MOVE     DSP-BASHKB         TO   DEN-F09.
     MOVE     DSP-TORICD         TO   DEN-F01.
     MOVE     DSP-JISH01(1)      TO   DEN-F02.
     MOVE     ZERO               TO   DEN-F051.
     MOVE     ZERO               TO   DEN-F03.
     MOVE     ZERO               TO   DEN-F04.
     START    HDENJNL   KEY  NOT  <  DEN-F09     DEN-F01
                                     DEN-F02     DEN-F051
                                     DEN-F03     DEN-F04
         INVALID   KEY
                   MOVE      9       TO   EOF-FLG
         NOT INVALID   KEY
              PERFORM  DEN-RD-SEC
     END-START.
     IF     ( EOF-FLG   =    9 )
     OR     ( DSP-JISH01(1)  NOT = ZERO
        AND   DEN-F02        NOT = DSP-JISH01(1)  )
              MOVE      7         TO   ERR-FLG
              GO   TO   CHK-JISHNO-EXIT
     END-IF.
*伝票Ｄ_ワーク
     PERFORM  DEN-MEI-SEC.
*ワーク_画面
     MOVE     1              TO   PCNT.
     PERFORM  MEI-DSP-SEC.
 CHK-JISHNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  伝票Ｄ_ワーク
*--------------------------------------------------------------*
 DEN-MEI-SEC             SECTION.
     INITIALIZE   MEISAI-WORK.
     PERFORM  VARYING  I  FROM  1  BY  1
                          UNTIL I  >  1000
                          OR    EOF-FLG = 9
*店舗ＣＤ
       MOVE     DEN-F07        TO   MEI-TENCD(I)
*店舗名
       MOVE     DSP-TORICD     TO   TEN-F52
       MOVE     DEN-F07        TO   TEN-F011
       READ     HTENMS
           INVALID   KEY
                MOVE NC"＊＊＊＊＊" TO   MEI-TENNM(I)
           NOT  INVALID   KEY
                MOVE TEN-F03        TO   MEI-TENNM(I)
       END-READ
*自社_
       MOVE     DEN-F02        TO   MEI-JISHNO(I)
*指定_
       IF       DSP-SHORKB  =  2
                MOVE DEN-F23     TO   MEI-SITENO(I)
                MOVE DEN-F23     TO   MEI-SAVNO (I)
       END-IF
*伝票Ｄリード
       MOVE      DEN-F02  TO  WK-F02
       PERFORM   DEN-RD-SEC   UNTIL  EOF-FLG = 9
                              OR     DEN-F02 NOT =  WK-F02
     END-PERFORM.
*ＭＡＸ行数
     COMPUTE   MAX-MCNT  =  I - 1.
*ＭＡＸ頁数
     DIVIDE    MAX-MCNT  BY  15  GIVING  MAX-PCNT
                                 REMAINDER  WK-AMARI.
     IF   WK-AMARI  NOT =  ZERO
          ADD  1    TO     MAX-PCNT
     END-IF.
 DEN-MEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ワーク_画面
*--------------------------------------------------------------*
 MEI-DSP-SEC             SECTION.
     COMPUTE  J  =    ( PCNT  -  1 ) * 15 + 1.
     MOVE     SPACE    TO       DSP-BODYX.
     PERFORM  VARYING  I  FROM  1  BY  1   UNTIL I  >  15
        MOVE  "M"      TO EDIT-OPTION  OF  DSP-SITE01(I)
     END-PERFORM.
     PERFORM  VARYING  I  FROM  1  BY  1
                          UNTIL I  >  15
                          OR    J  >  MAX-MCNT
        MOVE  MEI-JISHNO(J) TO DSP-JISH01(I)
        IF    MEI-SITENO(J) NOT =  ZERO
          MOVE  MEI-SITENO(J) TO DSP-SITE01(I)
        END-IF
        MOVE  MEI-TENCD(J)  TO DSP-TENC01(I)
        MOVE  MEI-TENNM(J)  TO DSP-TENN01(I)
        IF    MEI-ERRFLG(J)  NOT = ZERO
              MOVE  "R"      TO EDIT-OPTION  OF  DSP-SITE01(I)
        END-IF
        ADD   1               TO J
     END-PERFORM.
 MEI-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  画面_ワーク
*--------------------------------------------------------------*
 DSP-MEI-SEC             SECTION.
     COMPUTE  J   =  ( PCNT - 1 ) * 15 + 1.
     PERFORM  VARYING  I  FROM  1  BY  1
                          UNTIL I  >  15
                          OR    J  >  MAX-MCNT
        IF  DSP-SITE01(I) NUMERIC
            MOVE  DSP-SITE01(I)  TO MEI-SITENO(J)
        END-IF
        ADD   1   TO J
     END-PERFORM.
 DSP-MEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*  5               指定伝票番号入力　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 SITENO-SEC           SECTION.
     MOVE     G003           TO   DSP-GUIDE.
     PERFORM  DSP-WT-SEC.
     MOVE     "SITE"         TO   DSP-GRPX.
     MOVE     LCNT           TO   DSP-GRPY.
     PERFORM  DSP-RD-SEC.
*
     EVALUATE DSP-FNC
*取消
     WHEN     "F004"
                   MOVE      0         TO   SHORI-FLG
*明細終了
     WHEN     "F005"
                   PERFORM   DSP-MEI-SEC
                   PERFORM   CHK-PF05-SEC
                   IF   ERR-FLG  =  ZERO
                        MOVE      "Y"       TO   DSP-KAKU
                        MOVE      90        TO   SHORI-FLG
                   END-IF
*自社_へ
     WHEN     "F006"
                   MOVE      4         TO   SHORI-FLG
*前行
     WHEN     "F007"
                   IF   LCNT  >  1
                        SUBTRACT  1    FROM   LCNT   MCNT
                   END-IF
*次行
     WHEN     "F008"
                   IF   LCNT  <  15
                        ADD       1    TO     LCNT   MCNT
                   END-IF
*前頁
     WHEN     "F011"
                   PERFORM   DSP-MEI-SEC
                   IF   PCNT  >  1
                        SUBTRACT  1    FROM   PCNT
                        MOVE      1    TO     LCNT
                        COMPUTE   MCNT = ( PCNT - 1 ) * 15 + 1
                        PERFORM   MEI-DSP-SEC
                   END-IF
*次頁
     WHEN     "F012"
                   PERFORM   DSP-MEI-SEC
                   IF   PCNT  <  MAX-PCNT
                        ADD       1    TO     PCNT
                        MOVE      1    TO     LCNT
                        COMPUTE   MCNT = ( PCNT - 1 ) * 15 + 1
                        PERFORM   MEI-DSP-SEC
                   END-IF
*自動採番
     WHEN     "F016"
                   PERFORM   DSP-MEI-SEC
                   PERFORM   CHK-PF16-SEC
                   IF   ERR-FLG  =  ZERO
                     PERFORM   CHK-PF05-SEC
                     IF   ERR-FLG  =  ZERO
                        MOVE      "Y"       TO   DSP-KAKU
                        MOVE      90        TO   SHORI-FLG
                     END-IF
                   END-IF
     WHEN     "E000"
                   PERFORM   CHK-SITENO-SEC
                   IF   ERR-FLG  =  ZERO
                     IF   LCNT  <  15
                        ADD   1   TO   MCNT
                        IF    MCNT  >  MAX-MCNT
                              SUBTRACT  1  FROM  MCNT
                        ELSE
                              ADD       1  TO    LCNT
                        END-IF
                     END-IF
                   END-IF
     WHEN     OTHER
                   MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 SITENO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*  5               指定伝票_　チェック
*--------------------------------------------------------------*
 CHK-SITENO-SEC           SECTION.
     IF       DSP-SHORKB       = 2
     AND      DSP-SITE01(LCNT) NUMERIC
     AND      DSP-SITE01(LCNT) = ZERO
         MOVE     MEI-SAVNO(MCNT) TO   DSP-SITE01(LCNT)
     END-IF.
     IF       DSP-SITE01(LCNT)  NUMERIC
     AND      DSP-SITE01(LCNT)  NOT =  ZERO
         MOVE     TOK-F94        TO   LI-KBN
         MOVE     TOK-F92        TO   LI-KETA
         MOVE     TOK-F60        TO   LI-START
         MOVE     TOK-F61        TO   LI-END
         MOVE     DSP-SITE01(LCNT) TO   LI-DENNO
         CALL     "OSKTCDCK"     USING     LINK-AREA
         IF       LO-ERR  =  ZERO
                  MOVE "M" TO EDIT-OPTION OF DSP-SITE01(LCNT)
         ELSE
                  MOVE "R" TO EDIT-OPTION OF DSP-SITE01(LCNT)
                  MOVE  6  TO ERR-FLG
         END-IF
     END-IF.
 CHK-SITENO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  明細終了　　チェック
*--------------------------------------------------------------*
 CHK-PF05-SEC           SECTION.
*ワーク内チェックデジットＣＨＫ
     PERFORM   CHK-DEG-SEC.
*ワーク内ダブリＣＨＫ
     IF   ERR-FLG  =  ZERO
       PERFORM   CHK-MEI-SEC
     END-IF.
*伝票内ダブリＣＨＫ
     IF   ERR-FLG  =  ZERO
       PERFORM   CHK-DEN-SEC
     END-IF.
*ワーク_画面
     PERFORM   MEI-DSP-SEC.
 CHK-PF05-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  自動採番　　チェック
*--------------------------------------------------------------*
 CHK-PF16-SEC           SECTION.
*１行目ＣＨＫ
     IF   MCNT  <= 1
            MOVE      11      TO   ERR-FLG
            GO   TO   CHK-PF16-EXIT
     END-IF.
*直前ＣＨＫ
     COMPUTE   J   =   MCNT  -  1.
     MOVE      MEI-SITENO(J)    TO   LI-DENNO.
     MOVE      TOK-F94          TO   LI-KBN.
     MOVE      TOK-F92          TO   LI-KETA.
     MOVE      TOK-F60          TO   LI-START.
     MOVE      TOK-F61          TO   LI-END.
     CALL      "OSKTCDCK"       USING  LINK-AREA.
     IF   LO-ERR  NOT  =  0
          MOVE      12        TO   ERR-FLG
          GO   TO   CHK-PF16-EXIT
     END-IF.
*自動採番
     MOVE      MCNT     TO       J.
     PERFORM   VARYING  J  FROM  J  BY  1
               UNTIL  J  >  1000
               OR     J  >  MAX-MCNT
       MOVE      LO-NEXT        TO   MEI-SITENO(J)
       MOVE      LO-NEXT        TO   LI-DENNO
       MOVE      TOK-F94        TO   LI-KBN
       MOVE      TOK-F92        TO   LI-KETA
       MOVE      TOK-F60        TO   LI-START
       MOVE      TOK-F61        TO   LI-END
       CALL      "OSKTCDCK"     USING  LINK-AREA
       IF   LO-ERR  NOT  =  0
            DISPLAY   NC"伝票_採番エラー" UPON STAT
            MOVE      99        TO   SHORI-FLG
            GO   TO   CHK-PF16-EXIT
       END-IF
     END-PERFORM.
 CHK-PF16-EXIT.
     EXIT.
*--------------------------------------------------------------*
*  90              確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 KAKU-SEC          SECTION.
     MOVE     G004           TO   DSP-GUIDE.
     PERFORM  DSP-WT-SEC.
     MOVE     "KAKU"         TO   DSP-GRP.
     PERFORM  DSP-RD-SEC.
     EVALUATE DSP-FNC
     WHEN     "F004"
                        MOVE      0         TO   SHORI-FLG
     WHEN     "F006"
                        MOVE      5         TO   SHORI-FLG
     WHEN     "F015"
                        MOVE      1         TO   LCNT MCNT PCNT
                        PERFORM   MEI-DSP-SEC
                        MOVE      5         TO   SHORI-FLG
*前頁
     WHEN     "F011"
                   IF   PCNT  >  1
                        SUBTRACT  1    FROM   PCNT
                        MOVE      1    TO     LCNT
                        COMPUTE   MCNT = ( PCNT - 1 ) * 15 + 1
                        PERFORM   MEI-DSP-SEC
                   END-IF
*次頁
     WHEN     "F012"
                   IF   PCNT  <  MAX-PCNT
                        ADD       1    TO     PCNT
                        MOVE      1    TO     LCNT
                        COMPUTE   MCNT = ( PCNT - 1 ) * 15 + 1
                        PERFORM   MEI-DSP-SEC
                   END-IF
     WHEN     "E000"
                        PERFORM   DEN-UPDT-SEC
                        IF   ERR-FLG  =  ZERO
                             MOVE  ZERO  TO  SHORI-FLG
                        END-IF
     WHEN      OTHER
                        MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 KAKU-EXIT.
     EXIT.
*--------------------------------------------------------------*
*  90              伝票Ｄ　更新　処理
*--------------------------------------------------------------*
 DEN-UPDT-SEC          SECTION.
*確認ＣＨＫ
     IF       DSP-KAKU  NOT = "Y"
              MOVE      8         TO   ERR-FLG
              GO        TO        DEN-UPDT-SEC
     END-IF.
*更新
     PERFORM  VARYING  I  FROM  1  BY  1
                               UNTIL I  >  1000
                               OR    I > MAX-MCNT
*伝票Ｄスタート
       IF     MEI-SITENO(I)  NOT = MEI-SAVNO(I)
          MOVE     DSP-BASHKB     TO   DEN-F09
          MOVE     DSP-TORICD     TO   DEN-F01
          MOVE     MEI-JISHNO(I)  TO   DEN-F02
          MOVE     ZERO           TO   DEN-F051
                                       DEN-F03
                                       DEN-F04
          START    HDENJNL   KEY  >=   DEN-F09     DEN-F01
                                       DEN-F02     DEN-F051
                                       DEN-F03     DEN-F04
            INVALID   KEY
                        MOVE   9       TO   EOF-FLG
            NOT  INVALID   KEY
                        PERFORM   DEN-RD-SEC
          END-START
*自社_がブレイクするまで更新
          MOVE   MEI-JISHNO(I)    TO   WK-F02
          PERFORM          UNTIL EOF-FLG = 9
                           OR    DEN-F02 NOT = WK-F02
                 MOVE      MEI-SITENO(I)  TO   DEN-F23
                 MOVE      9              TO   DEN-F276
                 MOVE      9              TO   DEN-F27B
                 REWRITE   DEN-REC
                 PERFORM   DEN-RD-SEC
          END-PERFORM
       END-IF
     END-PERFORM.
 DEN-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 DSP-RD-SEC           SECTION.
     MOVE      "NE"           TO   DSP-PRO.
     READ     DSPFILE.
 DSP-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 DSP-WT-SEC       SECTION.
     IF       ERR-FLG  =  ZERO
              MOVE      SPACE               TO   DSP-ERR
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   DSP-ERR
     END-IF.
     MOVE     "ALLF"         TO   DSP-GRP.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    DSP-FSK150.
     MOVE     ZERO           TO   ERR-FLG.
 DSP-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 DEN-RD-SEC           SECTION.
     READ     HDENJNL   NEXT      AT   END
              MOVE      9         TO   EOF-FLG
              GO   TO   DEN-RD-EXIT
     END-READ.
*
     IF       DEN-F09  NOT  =  DSP-BASHKB
     OR       DEN-F01  NOT  =  DSP-TORICD
              MOVE      9         TO   EOF-FLG
              REWRITE   DEN-REC
              GO   TO   DEN-RD-EXIT
     END-IF.
*自社伝票_が９９９９を越えたら対象外
     IF       DEN-F02   >      99999999
              MOVE      9         TO   EOF-FLG
              REWRITE   DEN-REC
              GO   TO   DEN-RD-EXIT
     END-IF.
     IF       DEN-F274  =  1
     OR       DEN-F277  =  9
              GO   TO   DEN-RD-SEC
     END-IF.
     EVALUATE DSP-SHORKB
       WHEN   1
              IF   DEN-F27B  =  9
                   GO   TO   DEN-RD-SEC
              END-IF
       WHEN   2
              IF   DEN-F27B  =  0
                   GO   TO   DEN-RD-SEC
              END-IF
     END-EVALUATE.
 DEN-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   ワーク内チェックデジットＣＨＫ
*--------------------------------------------------------------*
 CHK-DEG-SEC            SECTION.
     PERFORM   VARYING  I  FROM  1  BY  1
               UNTIL  I  >  1000
               OR     I  >  MAX-MCNT
       IF MEI-SITENO(I) = ZERO
          MOVE      ZERO      TO   MEI-ERRFLG(I)
       ELSE
          MOVE      TOK-F94        TO   LI-KBN
          MOVE      TOK-F92        TO   LI-KETA
          MOVE      TOK-F60        TO   LI-START
          MOVE      TOK-F61        TO   LI-END
          MOVE      MEI-SITENO(I)  TO   LI-DENNO
          CALL      "OSKTCDCK"     USING     LINK-AREA
          IF   LO-ERR  =  ZERO
               MOVE      ZERO      TO   MEI-ERRFLG(I)
          ELSE
               MOVE      1         TO   MEI-ERRFLG(I)
               MOVE      6         TO   ERR-FLG
          END-IF
       END-IF
     END-PERFORM.
 CHK-DEG-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   ワーク内ダブリＣＨＫ
*--------------------------------------------------------------*
 CHK-MEI-SEC            SECTION.
*ワーク内ダブリＣＨＫ
     PERFORM   VARYING  I  FROM   1   BY  1
                           UNTIL  I > MAX-MCNT
       IF  MEI-SITENO(I) = ZERO
           MOVE ZERO TO  MEI-ERRFLG(I)
       ELSE
           COMPUTE   J  =   I  +  1
           PERFORM   VARYING  J  FROM  J   BY  1
                            UNTIL   J  >  MAX-MCNT
                 IF MEI-SITENO(I)  =      MEI-SITENO(J)
                         MOVE 1    TO  MEI-ERRFLG(I)
                         MOVE 1    TO  MEI-ERRFLG(J)
                         MOVE 1    TO  ERR-SW
                         MOVE 9    TO  ERR-FLG
                 END-IF
           END-PERFORM
       END-IF
     END-PERFORM.
 CHK-MEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票Ｄ内ダブリＣＨＫ
*--------------------------------------------------------------*
 CHK-DEN-SEC            SECTION.
*伝票Ｄ内ダブリＣＨＫ
     PERFORM   VARYING  I  FROM   1      BY  1
                           UNTIL  I > 1000
                           OR     I > MAX-MCNT
           IF      ( DSP-SHORKB    =    2
               AND   MEI-SITENO (I) NOT  =  MEI-SAVNO(I) )
           OR        DSP-SHORKB    =    1
              MOVE     ZERO             TO   EOF-FLG
              MOVE     DSP-TORICD       TO   DEN1-F01
              MOVE     MEI-SITENO (I)   TO   DEN1-F23
              MOVE     ZERO  TO   DEN1-F04 DEN1-F051 DEN1-F03
              START    HDENJNL1 KEY >=
                             DEN1-F01  DEN1-F23  DEN1-F04
                             DEN1-F051 DEN1-F03
                INVALID  KEY
                     MOVE     9        TO   EOF-FLG
                NOT  INVALID  KEY
                     READ     HDENJNL1  NEXT    AT   END
                              MOVE     9        TO   EOF-FLG
                     END-READ
              END-START
              IF       EOF-FLG  =  ZERO
              AND      MEI-SITENO(I) =  DEN1-F23
                       MOVE     1     TO   MEI-ERRFLG(I)
                       MOVE     10    TO   ERR-FLG
              ELSE
                       MOVE     ZERO  TO   MEI-ERRFLG(I)
              END-IF
           END-IF
     END-PERFORM.
 CHK-DEN-EXIT.
     EXIT.

```
