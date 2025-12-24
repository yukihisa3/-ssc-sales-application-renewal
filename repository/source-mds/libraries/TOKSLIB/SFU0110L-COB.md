# SFU0110L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SFU0110L.COB`

## ソースコード

```cobol
************************************************************
*   顧客名　　    ：   _サカタのタネ殿                    *
*   システム名    ：   在庫管理システム                    *
*   サブシステム名：   日次処理　                          *
*   プログラム名　：   振替チェックリスト                  *
*   プログラムID  ：   SFU0110L                            *
*   作成者        ：   ＮＡＶ　                            *
*   作成日        ：   2000.06.20                          *
*   更新日        ：                                       *
************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SFU0110L.
 AUTHOR.                NAV.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YA        IS   YA
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 振替ファイル         >>******************************
     SELECT   FURIKAF   ASSIGN    TO   DA-01-VI-FURIKAL2
                        ORGANIZATION         IS    INDEXED
                        ACCESS    MODE       IS    SEQUENTIAL
                        RECORD    KEY        IS    FUR-F03
                                                   FUR-F04
                                                   FUR-F05
                                                   FUR-F09
                        FILE    STATUS       IS    FUR-STATUS.
*****<< 取引先マスタ        >>-*****************************
     SELECT   HTOKMS    ASSIGN    TO DA-01-VI-TOKMS2
                        ORGANIZATION         IS    INDEXED
                        ACCESS    MODE       IS    DYNAMIC
                        RECORD    KEY        IS    TOK-F01
                        FILE      STATUS     IS    TOK-STATUS.
****<< 仕入先マスタ         >>******************************
     SELECT   ZSHIMS    ASSIGN  TO     DA-01-VI-ZSHIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS    MODE       IS   RANDOM
                        RECORD    KEY        IS   SHI-F01
                        FILE      STATUS     IS   SHI-STATUS.
****<< 倉庫マスタ           >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-STATUS.
****<< 商品名称マスタ       >>******************************
     SELECT   HMEIMS    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-F01
                        FILE    STATUS       IS   MEI-STATUS.
****<< 部門取引先マスタ     >>******************************
     SELECT   BUTOKMF   ASSIGN  TO   DA-01-VI-BUTOKML1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   BTO-F01
                        FILE    STATUS       IS   BTO-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
****<< 振替ファイル         >>******************************
 FD    FURIKAF
       LABEL     RECORD    IS    STANDARD.
       COPY      FURIKAF   OF    XFDLIB
                 JOINING   FUR   PREFIX.
****<< 取引先マスタ         >>******************************
 FD    HTOKMS
       LABEL     RECORD    IS    STANDARD.
       COPY      HTOKMS    OF    XFDLIB
                 JOINING   TOK   PREFIX.
****<< 仕入先マスタ         >>******************************
 FD    ZSHIMS
       LABEL     RECORD    IS    STANDARD.
       COPY      ZSHIMS    OF    XFDLIB
                 JOINING   SHI   PREFIX.
****<< 部門取引先マスタ     >>******************************
 FD    BUTOKMF
       LABEL     RECORD    IS    STANDARD.
       COPY      BUTOKMF   OF    XFDLIB
                 JOINING   BTO   PREFIX.
****<< 倉庫マスタ           >>******************************
 FD    ZSOKMS
       LABEL     RECORD    IS    STANDARD.
       COPY      ZSOKMS    OF    XFDLIB
                 JOINING   SOK   PREFIX.
****<< 商品名称マスタ       >>******************************
 FD    HMEIMS
       LABEL     RECORD    IS    STANDARD.
       COPY      HMEIMS    OF    XFDLIB
                 JOINING   MEI   PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF.
 01    PRT-REC               PIC  X(200).
****************************************************************
 WORKING-STORAGE        SECTION.
*
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 FUR-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
     02 SHI-STATUS           PIC  X(02).
     02 BTO-STATUS           PIC  X(02).
****  フラグ                  ****
 01  PSW-AREA.
     02  END-FLG             PIC  9(01)  VALUE  ZERO.
****  日付保存                ****
 01  SYSYMD                  PIC  9(06).
 01  SYSYMD-R                REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  カウンタ                ****
 01  CNT-AREA.
     02  L-CNT                   PIC  9(02)  VALUE  62.
     02  P-CNT                   PIC  9(07)  VALUE  ZERO.
     02  READ-CNT                PIC  9(07)  VALUE  ZERO.
     02  WRITE-CNT               PIC  9(07).
     02  SKIP-CNT                PIC  9(07).
****  ワーク                  ****
 01  NEW-DENNO                   PIC  X(09)  VALUE  SPACE.
 01  OLD-DENNO                   PIC  X(09)  VALUE  SPACE.
****  見出し行１             ****
 01  MIDASI-1.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  X(08)  VALUE "SFU0110L".
     03  FILLER              PIC  X(25)  VALUE  SPACE.
     03  FILLER              PIC  N(17)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　振替チェックリスト　＊＊＊".
     03  FILLER              PIC  X(15)  VALUE  SPACE.
     03  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  YA         VALUE  NC"処理日".
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-YY                PIC  99.
     03  FILLER              PIC  X(1)   VALUE  ".".
     03  H-MM                PIC  Z9.
     03  FILLER              PIC  X(1)   VALUE  ".".
     03  H-DD                PIC  Z9.
     03  FILLER              PIC  X(3)   VALUE  SPACE.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE  NC"頁".
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  PAGE-SUU            PIC  ZZ9.
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   YA.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"出荷日".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"伝区".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"伝票_".
     03  FILLER              PIC  X(06)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"振替".
     03  FILLER              PIC  X(05)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"振替名称".
     03  FILLER              PIC  X(26)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"場所".
     03  FILLER              PIC  X(42)  VALUE  SPACE.
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   YA.
     03  FILLER              PIC  X(15)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"行".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"入出".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"赤黒".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"商品コード".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"品　単".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  X(01)  VALUE  "(".
     03  FILLER              PIC  N(03)  VALUE  NC"_　番".
     03  FILLER              PIC  X(01)  VALUE  ")".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"商品名称".
     03  FILLER              PIC  X(44)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"数　量".
     03  FILLER              PIC  X(09)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"単　価".
****  見出し行４             ****
 01  MIDASI-4                CHARACTER   TYPE   IS   YA.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(67)  VALUE  ALL NC"─".
****  明細行０               ****
 01  MEISAI-0.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  X(134) VALUE  ALL   "-".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-HIZUKE1           PIC  99.
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  M-HIZUKE2           PIC  Z9.
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  M-HIZUKE3           PIC  Z9.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-DENKU             PIC  9(02).
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-DENNO             PIC  X(09).
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-MOTOCD            PIC  X(08).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-MOTONM            PIC  N(16).
     03  FILLER              PIC  X(10)  VALUE  SPACE.
     03  M-SAKICD            PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SAKINM            PIC  N(18).
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB.
     03  FILLER              PIC  X(09)  VALUE  SPACE.
     03  M-KANRIX            PIC  X(01)  VALUE  SPACE.
     03  M-KANRI             PIC  X(01)  VALUE  SPACE.
     03  M-KANRIY            PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-GYONO             PIC  9(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-IRIDE             PIC  N(02).
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-AKAKURO           PIC  X(01).
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-SHOCD             PIC  X(08).
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-HINTAN            PIC  X(08).
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-KAKKO1            PIC  X(01)  VALUE  "(".
     03  M-TANABAN           PIC  X(06).
     03  M-KAKKO2            PIC  X(01)  VALUE  ")".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-HINNM             PIC  N(30).
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-SURYO             PIC  --,---,--9.99.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-TANKA             PIC  --,---,--9.99.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZLIST   ".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
**振替ファイル
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  FURIKAF.
     MOVE   "FURIKAF  "       TO    ERR-FL-ID.
     MOVE    FUR-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**得意先マスタ　
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTOKMS.
     MOVE   "HTOKMS  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**倉庫マスタ
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**商品名称マスタ
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**仕入先マスタ
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSHIMS.
     MOVE   "ZSHIMS  "        TO    ERR-FL-ID.
     MOVE    SHI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**部門取引先マスタ
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BUTOKMF.
     MOVE   "BUTOKMF "        TO    ERR-FL-ID.
     MOVE    BTO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 GENERAL-PROCESS        SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    9
     PERFORM       END-SEC.
     STOP     RUN.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     FURIKAF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     ZSHIMS.
     OPEN     INPUT     BUTOKMF.
     OPEN     OUTPUT    PRINTF.
*---- システム日付　取得
     ACCEPT   SYSYMD    FROM   DATE.
     MOVE     SYS-YY    TO     H-YY.
     MOVE     SYS-MM    TO     H-MM.
     MOVE     SYS-DD    TO     H-DD.
***  振替ファイル初期ＲＥＡＤ
     READ     FURIKAF   AT     END
              MOVE      9      TO     END-FLG
          NOT AT END
              MOVE      FUR-F03   TO   OLD-DENNO
              MOVE      FUR-F03   TO   NEW-DENNO
              ADD       1         TO   READ-CNT
     END-READ.
*
 INIT-SEC-EXIT.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*
     IF       L-CNT         >=      62
              PERFORM       MIDASI-SEC
              PERFORM       MEISAI1-SEC
     END-IF.
*    伝票_ブレイク時明細１出力
*
     IF     ( OLD-DENNO     NOT =     NEW-DENNO )     AND
            ( L-CNT         NOT =     6 )
              PERFORM   MEISAI1-SEC
     END-IF.
*    明細２出力
*
*    入出名セット
     MOVE     FUR-F04        TO   M-GYONO.
     EVALUATE   FUR-F16
         WHEN   "1"
                MOVE    NC"入庫"  TO      M-IRIDE
         WHEN   "2"
                MOVE    NC"出庫"  TO      M-IRIDE
         WHEN   "3"
                MOVE    NC"発注"  TO      M-IRIDE
         WHEN   OTHER
                MOVE    NC"＊＊"  TO      M-IRIDE
     END-EVALUATE.
     MOVE     FUR-F05        TO        M-AKAKURO.
     MOVE     FUR-F10        TO        M-SHOCD
                                       MEI-F011.
     MOVE     FUR-F11        TO        M-HINTAN
                                       MEI-F012.
*    09/10/14  _番追加
*20100129 _番の印字　サカタ様の指示によりコマント ST
*    MOVE     FUR-F18        TO        M-TANABAN
*    IF       FUR-F18        NOT =     SPACE
*             MOVE    "("              TO       M-KAKKO1
*             MOVE    ")"              TO       M-KAKKO2
*    ELSE
*             MOVE     SPACE           TO       M-KAKKO1
*                                               M-KAKKO2
*    END-IF.
*20100129 _番の印字　サカタ様の指示によりコマント END
*
*    商品名称マスタＲＥＡＤ
     READ     HMEIMS
          INVALID      KEY
              MOVE    ALL NC"＊"  TO   M-HINNM
              MOVE    SPACE       TO   M-KANRIX
              MOVE    SPACE       TO   M-KANRI
              MOVE    SPACE       TO   M-KANRIY
          NOT INVALID  KEY
              MOVE    MEI-F02     TO   M-HINNM
              MOVE    "("         TO   M-KANRIX
              MOVE    MEI-F91     TO   M-KANRI
              MOVE    ")"         TO   M-KANRIY
     END-READ.
*
     MOVE     FUR-F13        TO        M-SURYO.
     MOVE     FUR-F14        TO        M-TANKA.
*
     WRITE    PRT-REC  FROM MEISAI-2  AFTER  1.
     ADD      1             TO        L-CNT.
*    振替ファイルＲＥＡＤ
     MOVE     NEW-DENNO     TO        OLD-DENNO.
     READ     FURIKAF   AT     END
              MOVE      9      TO     END-FLG
          NOT AT END
              MOVE      FUR-F03   TO   NEW-DENNO
              ADD       1         TO   READ-CNT
     END-READ.
*
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      2.1.1       見出し処理                              *
************************************************************
 MIDASI-SEC             SECTION.
     ADD      1  TO     P-CNT.
     MOVE     P-CNT     TO   PAGE-SUU.
     IF       P-CNT  = 1
              WRITE     PRT-REC FROM    MIDASI-1
              WRITE     PRT-REC FROM    MIDASI-4   AFTER  2
              WRITE     PRT-REC FROM    MIDASI-2   AFTER  1
              WRITE     PRT-REC FROM    MIDASI-3   AFTER  1
              WRITE     PRT-REC FROM    MIDASI-4   AFTER  1
        ELSE
              WRITE     PRT-REC FROM    MIDASI-1   AFTER  PAGE
              WRITE     PRT-REC FROM    MIDASI-4   AFTER  2
              WRITE     PRT-REC FROM    MIDASI-2   AFTER  1
              WRITE     PRT-REC FROM    MIDASI-3   AFTER  1
              WRITE     PRT-REC FROM    MIDASI-4   AFTER  1
     END-IF.
     MOVE     6  TO     L-CNT.
*
 MIDASI-SEC-EXIT.
     EXIT.
************************************************************
*                  明細１処理                              *
************************************************************
 MEISAI1-SEC            SECTION.
*
     IF  FUR-F16   =   "3"
*        発注の時は、発注日セット
         MOVE      FUR-F07(3:2)   TO   M-HIZUKE1
         MOVE      FUR-F07(5:2)   TO   M-HIZUKE2
         MOVE      FUR-F07(7:2)   TO   M-HIZUKE3
     ELSE
         MOVE      FUR-F09(3:2)   TO   M-HIZUKE1
         MOVE      FUR-F09(5:2)   TO   M-HIZUKE2
         MOVE      FUR-F09(7:2)   TO   M-HIZUKE3
     END-IF.
     MOVE     FUR-F02        TO   M-DENKU.
     MOVE     FUR-F03        TO   M-DENNO.
     MOVE     FUR-F06        TO   M-MOTOCD.
     MOVE     FUR-F17        TO   M-SAKICD.
*
*    振替元名セット
     IF  FUR-F20   =   "1"
         MOVE      FUR-F06        TO   TOK-F01
         READ      HTOKMS
             INVALID       KEY
                   MOVE SPACE     TO   M-MOTONM
             NOT INVALID   KEY
                   MOVE TOK-F02   TO   M-MOTONM
         END-READ
     END-IF.
     IF  FUR-F20   =   "2"
         MOVE      FUR-F06        TO   SHI-F01
         READ      ZSHIMS
             INVALID     KEY
                   MOVE      SPACE     TO   M-MOTONM
             NOT INVALID KEY
                   MOVE      SHI-F02   TO   M-MOTONM
         END-READ
     END-IF.
     IF  FUR-F20   =   "3"
         MOVE      FUR-F06        TO   BTO-F01
         READ      BUTOKMF
             INVALID       KEY
                   MOVE      SPACE     TO   M-MOTONM
             NOT INVALID   KEY
                   MOVE      BTO-F02   TO   M-MOTONM
         END-READ
     END-IF.
*    振替先（倉庫）名セット
     MOVE     FUR-F17     TO      SOK-F01.
*
     READ     ZSOKMS
           INVALID       KEY
              MOVE       SPACE   TO      M-SAKINM
           NOT INVALID   KEY
              MOVE       SOK-F02 TO      M-SAKINM
     END-READ.
*
     IF       L-CNT     >    6
              WRITE     PRT-REC   FROM MEISAI-0  AFTER  1
              ADD       1         TO   L-CNT
     END-IF.
     WRITE    PRT-REC   FROM      MEISAI-1 AFTER  1.
     ADD      1         TO        L-CNT.
*
 MEISAI1-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    FURIKAF HTOKMS ZSOKMS HMEIMS PRINTF ZSHIMS BUTOKMF.
     DISPLAY "READ  ｹﾝｽｳ = " READ-CNT  UPON  CONS.
     DISPLAY "PAGE    ｽｳ = " P-CNT     UPON  CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
