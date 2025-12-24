# ZNY0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZNY0020B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）                    *
*    業務名　　　　　：　在庫管理システム                      *
*    モジュール名　　：　作業実績チェックリスト                *
*    作成日　　　　　：  93/05/14                              *
*    作成者　　　　　：  NAV                                   *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZNY0020B.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          93/05/14.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE       IS   CONS
     STATION       IS   STA
         YB        IS   1-5PITCH
         YA        IS   2PITCH
         YB-21     IS   3PITCH.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 作業実績ファイル >>-*
     SELECT   ZSGYODT   ASSIGN    TO        DA-01-VI-ZSGYODT1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SGY-F01   SGY-F02
                        FILE      STATUS    SGY-ST1.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F011   MEI-F0121
                                            MEI-F0122  MEI-F0123
                        FILE      STATUS    MEI-ST1.
*----<< プリントファイル >>-*
     SELECT   PRTFILE   ASSIGN    TO        LP-04.
*----<< 画面ファイル >>-*
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        SYMBOLIC  DESTINATION        "DSP"
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING  MODE    DSP-PRO
                        SELECTED  FUNCTION  DSP-FNC
                        FILE      STATUS    DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 作業実績ファイル >>-*
 FD  ZSGYODT            LABEL     RECORD     IS  STANDARD.
     COPY     ZSGYODT1  OF   XFDLIB    JOINING   SGY  AS   PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            LABEL     RECORD     IS  STANDARD.
     COPY     JYOKEN1   OF   XFDLIB    JOINING   JYO  AS   PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS    OF   XFDLIB    JOINING   SOK  AS   PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS   PREFIX.
*----<< プリントファイル >>-*
 FD    PRTFILE          LINAGE    IS   66.
 01    PRT-REC                    PIC  X(200).
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     ZNY0020   OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)  VALUE  "ZNY0020B".
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT             PIC  X(08)  VALUE  SPACE.
     03  DSP-GRP             PIC  X(08)  VALUE  SPACE.
     03  DSP-PRO             PIC  X(02)  VALUE  SPACE.
     03  DSP-FNC             PIC  X(04)  VALUE  SPACE.
     03  DSP-ST.
       05  DSP-ST1           PIC  X(02)  VALUE  SPACE.
       05  DSP-ST2           PIC  X(04)  VALUE  SPACE.
     03  DSP-CON             PIC  X(06)  VALUE  SPACE.
     03  WK-GRP.
       05  WK-GRP-BODY       PIC  X(04)  VALUE  SPACE.
       05  WK-GRP-LINE       PIC  9(02)  VALUE  ZERO.
       05  FILLER            PIC  X(02)  VALUE  SPACE.
*----<< ｽﾃｰﾀｽ ｴﾘｱ >>-*
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  SGY-ST.
         05  SGY-ST1         PIC  X(02)  VALUE  SPACE.
         05  SGY-ST2         PIC  X(04)  VALUE  SPACE.
     03  JYO-ST.
         05  JYO-ST1         PIC  X(02)  VALUE  SPACE.
         05  JYO-ST2         PIC  X(04)  VALUE  SPACE.
     03  SOK-ST.
         05  SOK-ST1         PIC  X(02)  VALUE  SPACE.
         05  SOK-ST2         PIC  X(04)  VALUE  SPACE.
     03  MEI-ST.
         05  MEI-ST1         PIC  X(02)  VALUE  SPACE.
         05  MEI-ST2         PIC  X(04)  VALUE  SPACE.
*画面退避用
     COPY   ZNY0020  OF XMDLIB  JOINING   SAV  AS   PREFIX.
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
            NC"無効ＰＦキーです。".
     03  MSG02               PIC  N(20)  VALUE
            NC"Ｙを入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     03  MSG04               PIC  N(20)  VALUE
            NC"対象年月日に誤りがあります。".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       6.
*----<< ﾜｰｸ ｴﾘｱ >>-*
 01  WK-AREA.
     03  WK-BASYOM           PIC  N(05)  VALUE  SPACE.
     03  WK-BASYO            PIC  9(02)  VALUE  ZERO.
     03  WK-SEIREKI          PIC  9(04)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  99.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  A-CNT               PIC  9(03)  VALUE  ZERO.
     03  WK-TAISYOU          PIC  9(08)  VALUE  ZERO.
     03  WK-SJYO             PIC  X(01)  VALUE  SPACE.
 01  KEY-AREA.
     03  WK-SNO              PIC  9(07)  VALUE  ZERO.
     03  WK-KANSEI           PIC  9(08)  VALUE  ZERO.
     03  WK-SKUBUN           PIC  9(02)  VALUE  ZERO.
     03  WK-SBASYO           PIC  X(02)  VALUE  SPACE.
*----<< ｼｽﾃﾑ ﾋﾂﾞｹ･ｼﾞｶﾝ ｴﾘｱ >>-*
 01  SYS-DATE                PIC  9(06).
 01  FILLER                  REDEFINES   SYS-DATE.
     03  SYS-YY              PIC  9(02).
     03  SYS-MM              PIC  9(02).
     03  SYS-DD              PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>-*
 01  IN-MSG.
     03  FILLER              PIC  X(26)  VALUE
         "ｻｷﾞｮｳｼﾞｯｾｷﾌｧｲﾙ     (IN) = ".
     03  CNT-IN              PIC  ZZZZZZ9.
 01  SKIP-MSG.
     03  FILLER              PIC  X(26)  VALUE
         "ｻｷﾞｮｳｼﾞｯｾｷﾌｧｲﾙ   (SKIP) = ".
     03  CNT-SKIP            PIC  ZZZZZZ9.
 01  OUT-MSG.
     03  FILLER              PIC  X(26)  VALUE
         "ｻｷﾞｮｳｼﾞｯｾｷﾌｧｲﾙ    (OUT) = ".
     03  CNT-OUT             PIC  ZZZZZZ9.
 01  PAGE-MSG.
     03  FILLER              PIC  X(26)  VALUE
         "ﾁｪｯｸﾘｽﾄ       (ﾍﾟｰｼﾞｽｳ) = ".
     03  CNT-PAGE            PIC  ZZZZZZ9.
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE  ZERO.
*----<< ﾋﾂﾞｹﾁｪｯｸ ﾜｰｸ ｴﾘｱ >>-*
 01  CHK-DATE                PIC  9(08).
 01  FILLER                  REDEFINES      CHK-DATE.
     03  CHK-YYMM            PIC  9(06).
     03  FILLER              REDEFINES      CHK-YYMM.
         05  CHK-YY          PIC  9(04).
         05  CHK-MM          PIC  9(02).
     03  CHK-DD              PIC  9(02).
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(04).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  SHORI-F             PIC  9(02)  VALUE  ZERO.
****  見出し行１             ****
 01  MIDASI-1.
     03  FILLER              PIC  X(34)  VALUE  SPACE.
     03  FILLER              PIC  N(21)  VALUE
         NC"＊＊＊　作業実績入力チェックリスト　＊＊＊"
                             CHARACTER   TYPE   IS   3PITCH.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"処理日"
                             CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-YY                PIC  Z9.
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  H-MM                PIC  Z9.
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  H-DD                PIC  Z9.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"頁"
                             CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-PAGE              PIC  ZZZ9.
****  見出し行２             ****
 01  MIDASI-2                CHARACTER   TYPE   IS   2PITCH.
*****03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"作業_".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"完成日".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"作".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"場".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"担".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"行".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"入".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"_　番".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"商品".
     03  FILLER              PIC  X(05)  VALUE  "ｺｰﾄﾞ ".
     03  FILLER              PIC  N(03)  VALUE  NC"品　単".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"商　品　名".
     03  FILLER              PIC  X(39)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"数　量".
     03  FILLER              PIC  X(05)  VALUE  SPACE.
     03  FILLER              PIC  X(02)  VALUE  "ST".
     03  FILLER              PIC  N(01)  VALUE  NC"_".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"備　考".
****  見出し行３             ****
 01  MIDASI-3                CHARACTER   TYPE   IS   2PITCH.
*****03  FILLER              PIC  X(19)  VALUE  SPACE.
     03  FILLER              PIC  X(17)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"区".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"所".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"当".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"出".
****  明細行１               ****
 01  MEISAI-1.
*****03  FILLER              PIC  X(02).
     03  M-SGYNO             PIC  9(07).
     03  FILLER              PIC  X(01).
     03  M-SGYYMD-YY         PIC  Z9.
     03  M-TEN1              PIC  X(01).
     03  M-SGYYMD-MM         PIC  Z9.
     03  M-TEN2              PIC  X(01).
     03  M-SGYYMD-DD         PIC  Z9.
     03  FILLER              PIC  X(01).
**** 98/04/10  T.T ﾍﾝｺｳ
*****03  M-SGYKBN            PIC  9(02).
     03  M-SGYKBN            PIC  X(02).
     03  FILLER              PIC  X(01).
     03  M-BASYO             PIC  X(02).
     03  FILLER              PIC  X(01).
     03  M-TANTO             PIC  X(02).
     03  FILLER              PIC  X(01).
     03  M-GYNO-1            PIC  Z9.
     03  FILLER              PIC  X(01).
     03  M-NYSKBN-1          PIC  X.
     03  FILLER              PIC  X(02).
     03  M-TANA1-1           PIC  X(01).
     03  M-HAIHN1-1          PIC  X(01).
     03  M-TANA2-1           PIC  X(03).
     03  M-HAIHN2-1          PIC  X(01).
     03  M-TANA3-1           PIC  X(02).
     03  FILLER              PIC  X(01).
     03  M-HINCD-1           PIC  X(08).
     03  FILLER              PIC  X(01).
     03  M-HINTAN-1          PIC  X(08).
     03  FILLER              PIC  X(01).
     03  M-HINMEI1-1         PIC  N(15)
                             CHARACTER    TYPE   IS   1-5PITCH.
     03  M-HINMEI2-1         PIC  N(15)
                             CHARACTER    TYPE   IS   1-5PITCH.
     03  FILLER              PIC  X(01).
     03  M-SURYO-1           PIC  Z,ZZZ,ZZ9.99.
     03  FILLER              PIC  X(01).
     03  M-STNO-1            PIC  X(05).
     03  FILLER              PIC  X(01).
     03  M-BIKOU-1           PIC  X(10).
 01  MEISAI-2.
*****03  FILLER              PIC  X(03).
     03  FILLER              PIC  X(01).
     03  SAKJYO              PIC  N(02)
                             CHARACTER    TYPE   IS     2PITCH.
     03  FILLER              PIC  X(21).
     03  M-GYNO              PIC  Z9.
     03  FILLER              PIC  X(01).
     03  M-NYSKBN            PIC  X.
     03  FILLER              PIC  X(02).
     03  M-TANA1             PIC  X(01).
     03  M-HAIHN1            PIC  X(01).
     03  M-TANA2             PIC  X(03).
     03  M-HAIHN2            PIC  X(01).
     03  M-TANA3             PIC  X(02).
     03  FILLER              PIC  X(01).
     03  M-HINCD             PIC  X(08).
     03  FILLER              PIC  X(01).
     03  M-HINTAN            PIC  X(08).
     03  FILLER              PIC  X(01).
     03  M-HINMEI1           PIC  N(15)
                             CHARACTER    TYPE   IS   1-5PITCH.
     03  M-HINMEI2           PIC  N(15)
                             CHARACTER    TYPE   IS   1-5PITCH.
     03  FILLER              PIC  X(01).
     03  M-SURYO             PIC  Z,ZZZ,ZZ9.99.
     03  FILLER              PIC  X(01).
     03  M-STNO              PIC  X(05).
     03  FILLER              PIC  X(01).
     03  M-BIKOU             PIC  X(10).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*---<< LINK AREA >>-*
 LINKAGE                   SECTION.
 01  LINK-AREA.
     03  LINK-WSMEI          PIC  X(08).
*
******************************************************************
 PROCEDURE         DIVISION      USING     LINK-AREA.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  作業実績ファイル　--------------------------------*
 SGY-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSGYODT.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"作業実績ファイル異常！"
              "ST1=" SGY-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   条件ファイル　-----------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   倉庫マスタ -----------------------------------*
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSOKMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"倉庫マスタ異常！"
              "ST1=" SOK-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG  =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-RTN           SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " START "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STA.
* ファイル　ＯＰＥＮ
     OPEN     INPUT     ZSGYODT.
     OPEN     INPUT     HJYOKEN.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
* 西暦の取得
     MOVE     "57"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG    =   1
              DISPLAY   "HJYOKEN INV KEY=57"  UPON STA
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F04        TO   WK-SEIREKI.
* 場所の取得
     MOVE     "65"           TO   JYO-F01.
     MOVE     LINK-WSMEI     TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG   =    1
              DISPLAY   "HJYOKEN INV KEY=65"  UPON STA
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F04        TO   WK-BASYO.
     IF       WK-BASYO  NOT  =    1
              MOVE  WK-BASYO      TO   BASYO
              MOVE  WK-BASYO      TO   SOK-F01
              PERFORM   SOK-READ-RTN
              IF        INV-FLG   =    0
                        MOVE  SOK-F02  TO   WK-BASYOM
              ELSE
                        MOVE  SPACE    TO   WK-BASYOM
              END-IF
     END-IF.
*
     MOVE     0              TO   SHORI-F.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-RTN           SECTION.
     EVALUATE    SHORI-F
       WHEN      0      PERFORM   DSP-INIT-RTN
       WHEN      1      PERFORM   DSP-GRP003-RTN
       WHEN      2      PERFORM   DSP-KAKNIN-RTN
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-RTN            SECTION.
     CLOSE              ZSGYODT
                        HJYOKEN
                        ZSOKMS
                        HMEIMS
                        DSPFILE
                        PRTFILE.
*
     MOVE        IN-CNT      TO    CNT-IN.
     MOVE        SKIP-CNT    TO    CNT-SKIP.
     MOVE        OUT-CNT     TO    CNT-OUT.
     MOVE        PAGE-CNT    TO    CNT-PAGE.
     DISPLAY     IN-MSG      UPON  CONS.
     DISPLAY     SKIP-MSG    UPON  CONS.
     DISPLAY     OUT-MSG     UPON  CONS.
     DISPLAY     PAGE-MSG    UPON  CONS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STA.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    初期画面表示処理                            *
*--------------------------------------------------------------*
 DSP-INIT-RTN           SECTION.
     MOVE     SPACE          TO   ZNY0020.
     IF       WK-BASYO     NOT =  1
              MOVE     WK-BASYO       TO   BASYO
              MOVE     WK-BASYOM      TO   BASYOM
     END-IF.
     MOVE     SYS-YY         TO   TNEN.
     MOVE     SYS-MM         TO   TTUKI.
     MOVE     SYS-DD         TO   THI.
*属性クリア
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE     "ZNY0020"      TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
*
     MOVE     1              TO   SHORI-F.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　入力                              *
*--------------------------------------------------------------*
 DSP-GRP003-RTN     SECTION.
     MOVE     G001           TO   GUIDE.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "E000"
              PERFORM   CHK-BODY-RTN
              IF  ERR-FLG         =    ZERO
                  MOVE  "Y"       TO   KAKNIN
                  MOVE  2         TO   SHORI-F
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-GRP003-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　チェック                          *
*--------------------------------------------------------------*
 CHK-BODY-RTN       SECTION.
*属性クリア
     PERFORM  CLR-BODY-RTN.
*作業場所チェック
     IF       WK-BASYO     NOT =     1
              GO       TO       CHK-GRP003-01
     END-IF.
     MOVE     BASYO     TO        SOK-F01.
     PERFORM  SOK-READ-RTN.
     IF       INV-FLG   =    1
              MOVE      3    TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF BASYO
              MOVE     "R"        TO   EDIT-OPTION OF BASYO
              GO   TO   CHK-GRP003-01
     END-IF.
     MOVE     SOK-F02             TO   BASYOM.
*対象年月日
 CHK-GRP003-01.
     IF       TNEN     IS NOT     NUMERIC
              MOVE      0    TO   TNEN
     END-IF.
     IF       TTUKI    IS NOT     NUMERIC
              MOVE      0    TO   TTUKI
     END-IF.
     IF       THI      IS NOT     NUMERIC
              MOVE      0    TO   THI
     END-IF.
*  日付論理チェック
     MOVE     TNEN      TO        WK-TAISYOU(3:2)
     MOVE     TTUKI     TO        WK-TAISYOU(5:2)
     MOVE     THI       TO        WK-TAISYOU(7:2)
     MOVE     WK-TAISYOU     TO   CHK-DATE.
     PERFORM  YMD-CHK-RTN.
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KAKNIN-RTN      SECTION.
     PERFORM  CLR-BODY-RTN.
     MOVE     G002                TO   GUIDE.
     MOVE     "GRP002"            TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              PERFORM   CLR-TAIL-RTN
              MOVE      1         TO   SHORI-F
     WHEN     "E000"
              PERFORM   CHK-KAKNIN-RTN
              IF   ERR-FLG   =    0
                   PERFORM   PRT-WR-RTN
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　チェック                              *
*--------------------------------------------------------------*
 CHK-KAKNIN-RTN       SECTION.
*属性クリア
     PERFORM  CLR-BODY-RTN.
*
     IF       KAKNIN   NOT =      "Y"
              MOVE      2    TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KAKNIN
     END-IF.
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面ＲＥＡＤ　　　　                        *
*--------------------------------------------------------------*
 DSP-RD-RTN           SECTION.
     IF       ERR-FLG   =    0
              MOVE      SPACE              TO   GMSG
     ELSE
              MOVE      MSG-TBL(ERR-FLG)   TO   GMSG
     END-IF.
     MOVE     "SCREEN"  TO      DSP-GRP.
     PERFORM  DSP-WR-RTN.
*
     IF       ERR-FLG   NOT =   0
              MOVE      "AL"    TO   DSP-PRO
              MOVE      0       TO   ERR-FLG
     ELSE
              MOVE      "NE"    TO   DSP-PRO
     END-IF.
*
     MOVE     WK-GRP            TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE             TO   DSP-PRO.
 DSP-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL      画面ＷＲＩＴＥ　                           *
*--------------------------------------------------------------*
 DSP-WR-RTN             SECTION.
     MOVE     SPACE        TO   DSP-PRO.
     WRITE    ZNY0020.
 DSP-WR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     日付チェック　　                            *
*--------------------------------------------------------------*
 YMD-CHK-RTN            SECTION.
*## 1999/12/20 NAV
*****COMPUTE  CHK-YY    =   WK-SEIREKI  +  CHK-YY.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     CHK-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   CHK-DATE.
     DIVIDE   CHK-YY   BY   4  GIVING   CHK-01  REMAINDER  CHK-02.
     IF       CHK-02    =   0
              MOVE     29         TO    WK-MATUBI(2)
     ELSE
              MOVE     28         TO    WK-MATUBI(2)
     END-IF.
     IF       CHK-MM    =   0     OR    >   12
              IF   ERR-FLG  =     0
                   MOVE     4     TO    ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF TTUKI
              MOVE     "R"        TO   EDIT-OPTION OF TTUKI
     END-IF.
     IF       CHK-DD    =   0
     OR       CHK-DD    >  WK-MATUBI(CHK-MM)
              IF   ERR-FLG  =     0
                   MOVE     4     TO    ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF THI
              MOVE     "R"        TO   EDIT-OPTION OF THI
     END-IF.
 YMD-CHK-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF BASYO
                                  EDIT-CURSOR OF TNEN
                                  EDIT-CURSOR OF TTUKI
                                  EDIT-CURSOR OF THI.
     MOVE     "M"            TO   EDIT-OPTION OF BASYO
                                  EDIT-OPTION OF TNEN
                                  EDIT-OPTION OF TTUKI
                                  EDIT-OPTION OF THI.
     IF    WK-BASYO   =      1
           MOVE      " "     TO   EDIT-STATUS OF BASYO
     ELSE
           MOVE      "X"     TO   EDIT-STATUS OF BASYO
     END-IF.
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF KAKNIN.
     MOVE     "M"            TO   EDIT-OPTION OF KAKNIN.
 CLR-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          帳票出力処理                            *
*--------------------------------------------------------------*
 PRT-WR-RTN             SECTION.
     MOVE     SYS-YY                 TO   H-YY.
     MOVE     SYS-MM                 TO   H-MM.
     MOVE     SYS-DD                 TO   H-DD.
     MOVE     TNEN                   TO   WK-TAISYOU(3:2).
     MOVE     TTUKI                  TO   WK-TAISYOU(5:2).
     MOVE     THI                    TO   WK-TAISYOU(7:2).
*## 1999/12/20 NAV
*****COMPUTE  WK-TAISYOU  =  WK-SEIREKI * 10000 + WK-TAISYOU.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-TAISYOU          TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-TAISYOU.
     DISPLAY "WK-TAISYOU = " WK-TAISYOU UPON CONS.
     PERFORM       SGY-READ-RTN.
     IF       END-FLG    NOT =      "END"
              MOVE     SPACE         TO   MEISAI-1
              MOVE     SPACE         TO   MEISAI-2
     END-IF.
     PERFORM       HENSYU-RTN   UNTIL  END-FLG = "END".
     IF       LINE-CNT      =     7         AND
              WK-SJYO       =     1
         MOVE      NC"削除"       TO        SAKJYO
         WRITE     PRT-REC        FROM      MEISAI-2    AFTER 1
         MOVE      SPACE          TO        MEISAI-2
     END-IF.
 PRT-WR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3         編集印字　処理                           *
*--------------------------------------------------------------*
 HENSYU-RTN             SECTION.
     IF       LINE-CNT      >=      64
              PERFORM       MIDASI-RTN
     END-IF.
     IF       WK-SNO         =     SGY-F01   AND
              WK-KANSEI      =     SGY-F05   AND
              WK-SKUBUN      =     SGY-F03   AND
              WK-SBASYO      =     SGY-F04   AND
              LINE-CNT   NOT =     5
       PERFORM     TENSO1-RTN
       WRITE       PRT-REC   FROM      MEISAI-2   AFTER  1
       MOVE        SPACE     TO        MEISAI-2
       ADD         1         TO        LINE-CNT
      ELSE
       PERFORM     TENSO2-RTN
       IF          LINE-CNT   =        7     AND
                   WK-SJYO    =        1
          WRITE    PRT-REC   FROM      MEISAI-1   AFTER   1
          MOVE     SPACE     TO        MEISAI-1
          ADD      2         TO        LINE-CNT
        ELSE
          WRITE    PRT-REC   FROM      MEISAI-1   AFTER 2
          MOVE     SPACE     TO        MEISAI-1
          ADD      2         TO        LINE-CNT
       END-IF
      MOVE     SGY-F01       TO        WK-SNO
      MOVE     SGY-F05       TO        WK-KANSEI
      MOVE     SGY-F03       TO        WK-SKUBUN
      MOVE     SGY-F04       TO        WK-SBASYO
      MOVE     SGY-F97       TO        WK-SJYO
     END-IF.
*
     PERFORM       SGY-READ-RTN.
*
 HENSYU-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細１転送処理　                            *
*--------------------------------------------------------------*
*明細転送
 TENSO1-RTN             SECTION.
     IF       A-CNT     =    2    AND
              SGY-F97   =    1
       MOVE   NC"削除"  TO        SAKJYO
      ELSE
       MOVE   SPACE     TO        SAKJYO
     END-IF.
     PERFORM  TENSO3-RTN.
 TENSO1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細２転送処理　                            *
*--------------------------------------------------------------*
 TENSO2-RTN             SECTION.
     MOVE     1         TO        A-CNT.
     IF       LINE-CNT      =     7    AND
              WK-SJYO       =     1
         MOVE      NC"削除"       TO        SAKJYO
         WRITE     PRT-REC        FROM      MEISAI-2    AFTER 1
         MOVE      SPACE          TO        MEISAI-2
     END-IF.
     IF            LINE-CNT       =    62
            AND    SGY-F97        =    1
         PERFORM   MIDASI-RTN
     END-IF.
     IF            LINE-CNT       =    63
         PERFORM   MIDASI-RTN
     END-IF.
     MOVE     SGY-F01        TO   M-SGYNO.
     IF       SGY-F05   NOT  =    0      OR
              SGY-F05   NOT  =    SPACE
         MOVE      "."       TO   M-TEN1
         MOVE      "."       TO   M-TEN2
     END-IF.
     MOVE     SGY-F05(3:2)   TO   M-SGYYMD-YY.
     MOVE     SGY-F05(5:2)   TO   M-SGYYMD-MM.
     MOVE     SGY-F05(7:2)   TO   M-SGYYMD-DD.
     MOVE     SGY-F03        TO   M-SGYKBN.
     MOVE     SGY-F04        TO   M-BASYO.
     PERFORM  TENSO3-1-RTN.
     MOVE     SGY-F14        TO   M-TANTO.
 TENSO2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細３転送処理　                            *
*--------------------------------------------------------------*
 TENSO3-RTN             SECTION.
     MOVE     SGY-F02        TO   M-GYNO.
     MOVE     SGY-F06        TO   M-NYSKBN.
     IF       SGY-F10   NOT  =    SPACE
         MOVE      "-"       TO   M-HAIHN1
         MOVE      "-"       TO   M-HAIHN2
     END-IF.
     MOVE     SGY-F10(1:1)   TO   M-TANA1.
     MOVE     SGY-F10(2:3)   TO   M-TANA2.
     MOVE     SGY-F10(5:2)   TO   M-TANA3.
     MOVE     SGY-F08        TO   M-HINCD.
     MOVE     SGY-F09        TO   M-HINTAN.
*商品名索引
     MOVE     SGY-F08        TO   MEI-F011.
     MOVE     SGY-F09(1:5)   TO   MEI-F0121.
     MOVE     SGY-F09(6:2)   TO   MEI-F0122.
     MOVE     SGY-F09(8:1)   TO   MEI-F0123.
     PERFORM  MEI-READ-RTN.
     IF       INV-FLG      =      0
              MOVE    MEI-F021    TO        M-HINMEI1
              MOVE    MEI-F022    TO        M-HINMEI2
     ELSE
              MOVE    SPACE       TO        M-HINMEI1
              MOVE    SPACE       TO        M-HINMEI2
     END-IF.
     MOVE     SGY-F11        TO   M-SURYO.
     MOVE     SGY-F07        TO   M-STNO.
     MOVE     SGY-F12        TO   M-BIKOU.
 TENSO3-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細３－１転送処理                          *
*--------------------------------------------------------------*
 TENSO3-1-RTN           SECTION.
     MOVE     SGY-F02        TO   M-GYNO-1.
     MOVE     SGY-F06        TO   M-NYSKBN-1.
     IF       SGY-F10   NOT  =    SPACE
         MOVE      "-"       TO   M-HAIHN1-1
         MOVE      "-"       TO   M-HAIHN2-1
     END-IF.
     MOVE     SGY-F10(1:1)   TO   M-TANA1-1.
     MOVE     SGY-F10(2:3)   TO   M-TANA2-1.
     MOVE     SGY-F10(5:2)   TO   M-TANA3-1.
     MOVE     SGY-F08        TO   M-HINCD-1.
     MOVE     SGY-F09        TO   M-HINTAN-1.
*商品名索引
     MOVE     SGY-F08        TO   MEI-F011.
     MOVE     SGY-F09(1:5)   TO   MEI-F0121.
     MOVE     SGY-F09(6:2)   TO   MEI-F0122.
     MOVE     SGY-F09(8:1)   TO   MEI-F0123.
     PERFORM  MEI-READ-RTN.
     IF       INV-FLG      =      0
              MOVE    MEI-F021    TO        M-HINMEI1-1
              MOVE    MEI-F022    TO        M-HINMEI2-1
     ELSE
              MOVE    SPACE       TO        M-HINMEI1-1
              MOVE    SPACE       TO        M-HINMEI2-1
     END-IF.
     MOVE     SGY-F11        TO   M-SURYO-1.
     MOVE     SGY-F07        TO   M-STNO-1.
     MOVE     SGY-F12        TO   M-BIKOU-1.
 TENSO3-1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.1       見出し処理                              *
*----------------------------------------------------------*
 MIDASI-RTN             SECTION.
     MOVE         SPACE      TO      PRT-REC.
     ADD          1          TO      PAGE-CNT.
     MOVE         PAGE-CNT   TO      H-PAGE.
     IF     PAGE-CNT      NOT =      1
            WRITE  PRT-REC   AFTER   PAGE
     END-IF.
     WRITE         PRT-REC   FROM    MIDASI-1   AFTER  2.
     WRITE         PRT-REC   FROM    MIDASI-2   AFTER  2.
     WRITE         PRT-REC   FROM    MIDASI-3   AFTER  1.
     MOVE          5         TO      LINE-CNT.
 MIDASI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品名称マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 MEI-READ-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HMEIMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-READ-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   倉庫マスタ　　　ＲＥＡＤ                     *
*--------------------------------------------------------------*
 SOK-READ-RTN           SECTION.
     MOVE     0              TO   INV-FLG.
     READ     ZSOKMS   INVALID  KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   作業実績マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-READ-RTN           SECTION.
     READ     ZSGYODT
         AT   END
              MOVE      "END"     TO   END-FLG
              GO        TO        SGY-READ-EXIT
     END-READ.
     ADD      1         TO        IN-CNT.
     IF       BASYO       NOT =   SGY-F04
              ADD       1         TO   SKIP-CNT
              GO        TO        SGY-READ-RTN
     END-IF.
     IF       WK-TAISYOU  NOT =   SGY-F98  AND
              WK-TAISYOU  NOT =   SGY-F99
              ADD       1         TO   SKIP-CNT
              GO        TO        SGY-READ-RTN
     END-IF.
     ADD      1         TO   A-CNT.
     ADD      1         TO   OUT-CNT.
 SGY-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
