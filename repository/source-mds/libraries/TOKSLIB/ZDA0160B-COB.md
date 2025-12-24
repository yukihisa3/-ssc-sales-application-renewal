# ZDA0160B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZDA0160B.COB`

## ソースコード

```cobol
************************************************************
*   顧客名　　    ：   _サカタのタネ殿                    *
*   システム名    ：   在庫管理システム                    *
*   サブシステム名：   日次処理　                          *
*   プログラム名　：   振替データリスト                    *
*   プログラムID  ：   ZDA0160B                            *
*   作成者        ：   ＮＡＶ　                            *
*   作成日        ：   1993.05.03                          *
*   更新日        ：                                       *
************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            ZDA0160B.
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
     SELECT   INFILE    ASSIGN  TO   DA-01-S-INFILE
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   IN-STATUS.
*****<< 取引先マスタ        >>-*****************************
     SELECT   HTOKMS    ASSIGN    TO DA-01-VI-TOKMS3
                        ORGANIZATION         IS    INDEXED
                        ACCESS    MODE       IS    DYNAMIC
                        RECORD    KEY        IS    TOK-F52
                        FILE      STATUS     IS    TOK-STATUS.
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
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
****<< 振替ファイル         >>******************************
 FD    INFILE.
*
 01  IN-REC.
     03  IN-F01                  PIC  9(04).
     03  IN-F02                  PIC  9(02).
     03  IN-F03                  PIC  X(09).
     03  IN-F04                  PIC  9(02).
     03  IN-F05                  PIC  9(01).
     03  IN-F06                  PIC  X(08).
     03  IN-F07                  PIC  9(05).
     03  IN-F08                  PIC  9(08).
     03  IN-F09                  PIC  9(08).
     03  IN-F10                  PIC  9(08).
     03  IN-F11                  PIC  X(08).
     03  IN-F12                  PIC  X(08).
     03  IN-F13                  PIC  X(05).
     03  IN-F14                  PIC  X(01).
     03  IN-F15                  PIC  X(01).
     03  IN-F16                  PIC S9(07)V99.
     03  IN-F17                  PIC S9(07)V99.
     03  IN-F18                  PIC S9(09).
     03  IN-F19                  PIC  X(02).
     03  IN-F20                  PIC  X(01).
     03  IN-F21                  PIC  X(01).
     03  IN-F22                  PIC  X(02).
     03  IN-F23                  PIC  X(10).
     03  IN-F24                  PIC  X(07).
     03  IN-F25                  PIC  X(13).
     03  IN-F26                  PIC  9(02).
     03  IN-F27                  PIC  9(01).
     03  IN-F28                  PIC  9(08).
     03  IN-F29                  PIC  X(08).
     03  IN-F30                  PIC  X(01).
     03  IN-F31                  PIC S9(07)V99.
     03  IN-F32                  PIC S9(09).
     03  IN-F33                  PIC  X(06).
     03  IN-F34                  PIC  X(09).
     03  IN-F90                  PIC  X(62).
*
****<< 取引先マスタ         >>******************************
 FD    HTOKMS
       LABEL     RECORD    IS    STANDARD.
       COPY      HTOKMS    OF    XFDLIB
                 JOINING   TOK   PREFIX.
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
     02 IN-STATUS            PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
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
     02  P-CNT                   PIC  9(03)  VALUE  ZERO.
     02  READ-CNT                PIC  9(07)  VALUE  ZERO.
     02  WRITE-CNT               PIC  9(07).
     02  SKIP-CNT                PIC  9(07).
****  ワーク                  ****
 01  NEW-DENNO                   PIC  9(07)  VALUE  ZERO.
 01  OLD-DENNO                   PIC  9(07)  VALUE  ZERO.
****  見出し行１             ****
 01  MIDASI-1.
     03  FILLER              PIC  X(34)  VALUE  SPACE.
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
     03  FILLER              PIC  N(02)  VALUE  NC"日付".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"伝区".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"伝票_".
     03  FILLER              PIC  X(09)  VALUE  SPACE.
**** 03  FILLER              PIC  N(01)  VALUE  NC"行".
**** 03  FILLER              PIC  X(01)  VALUE  SPACE.
**** 03  FILLER              PIC  N(02)  VALUE  NC"入出".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"振替元".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"振替元名称".
     03  FILLER              PIC  X(15)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"入庫先".
     03  FILLER              PIC  X(44)  VALUE  SPACE.
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   YA.
     03  FILLER              PIC  X(21)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"行".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"入出".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"商品コード".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"品　単".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"商品名称".
     03  FILLER              PIC  X(44)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"数　量".
     03  FILLER              PIC  X(09)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"単　価".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-HIZUKE1           PIC  Z9.
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  M-HIZUKE2           PIC  Z9.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-DENKU             PIC  9(02).
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  M-DENNO             PIC  9(07).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
***  03  M-GYONO             PIC  9(02).
***  03  FILLER              PIC  X(01)  VALUE  SPACE.
***  03  M-IRIDE             PIC  N(02).
     03  FILLER              PIC  X(09)  VALUE  SPACE.
     03  M-MOTOCD            PIC  X(08).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-MOTONM            PIC  N(16).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SAKICD            PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SAKINM            PIC  N(18).
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB.
     03  FILLER              PIC  X(21)  VALUE  SPACE.
     03  M-GYONO             PIC  9(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-IRIDE             PIC  N(02).
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-SHOCD             PIC  X(08).
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-HINTAN            PIC  X(08).
     03  FILLER              PIC  X(02)  VALUE  SPACE.
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
                   PROCEDURE  INFILE.
     MOVE   "INFILE  "        TO    ERR-FL-ID.
     MOVE    IN-STATUS        TO    ERR-STCD.
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
     OPEN     INPUT     INFILE.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
     OPEN     OUTPUT    PRINTF.
*---- システム日付　取得
     ACCEPT   SYSYMD    FROM   DATE.
     MOVE     SYS-YY    TO     H-YY.
     MOVE     SYS-MM    TO     H-MM.
     MOVE     SYS-DD    TO     H-DD.
***  振替ファイル初期ＲＥＡＤ
     READ     INFILE    AT     END
              MOVE      9      TO     END-FLG
          NOT AT END
              MOVE      IN-F03 TO     OLD-DENNO
              MOVE      IN-F03 TO     NEW-DENNO
              ADD       1      TO     READ-CNT
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
            ( L-CNT         NOT =     8 )
              PERFORM       MEISAI1-SEC
     END-IF.
*    明細２出力
*
*    入出名セット
     MOVE     IN-F04     TO      M-GYONO.
     EVALUATE   IN-F20
         WHEN   "1"
                MOVE    NC"入庫"  TO      M-IRIDE
         WHEN   "2"
                MOVE    NC"出庫"  TO      M-IRIDE
         WHEN   OTHER
                MOVE    SPACE     TO      M-IRIDE
     END-EVALUATE.
     MOVE     IN-F11        TO        M-SHOCD
                                      MEI-F011.
     MOVE     IN-F12        TO        M-HINTAN
                                      MEI-F012.
*    商品名称マスタＲＥＡＤ
     READ     HMEIMS
          INVALID      KEY
              MOVE    ALL NC"＊"  TO   M-HINNM
          NOT INVALID  KEY
              MOVE     MEI-F02    TO   M-HINNM
     END-READ.
*
     MOVE     IN-F16        TO        M-SURYO.
     MOVE     IN-F17        TO        M-TANKA.
*
     WRITE    PRT-REC  FROM MEISAI-2  AFTER  1.
     ADD      1             TO        L-CNT.
*    振替ファイルＲＥＡＤ
     MOVE     NEW-DENNO     TO        OLD-DENNO.
     READ     INFILE    AT     END
              MOVE      9      TO     END-FLG
          NOT AT END
              MOVE      IN-F03 TO     NEW-DENNO
              ADD       1      TO     READ-CNT
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
              WRITE     PRT-REC FROM    MIDASI-2   AFTER  3
              WRITE     PRT-REC FROM    MIDASI-3   AFTER  1
        ELSE
              WRITE     PRT-REC FROM    MIDASI-1   AFTER  PAGE
              WRITE     PRT-REC FROM    MIDASI-2   AFTER  3
              WRITE     PRT-REC FROM    MIDASI-3   AFTER  1
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
     MOVE     IN-F10(5:2) TO     M-HIZUKE1.
     MOVE     IN-F10(7:2) TO     M-HIZUKE2.
     MOVE     IN-F02     TO      M-DENKU.
     MOVE     IN-F03     TO      M-DENNO.
**** MOVE     IN-F04     TO      M-GYONO.
     MOVE     IN-F06     TO      M-MOTOCD.
     MOVE     IN-F19     TO      M-SAKICD.
*    入出名セット
***  EVALUATE   IN-F20
***      WHEN   "1"
***             MOVE    NC"入庫"  TO      M-IRIDE
***      WHEN   "2"
***             MOVE    NC"出庫"  TO      M-IRIDE
***      WHEN   OTHER
***             MOVE    SPACE     TO      M-IRIDE
***  END-EVALUATE.
*    振替元名セット
     MOVE     IN-F06     TO      TOK-F52.
*
     READ     HTOKMS
           INVALID       KEY
              MOVE       SPACE   TO      M-MOTONM
           NOT INVALID   KEY
              MOVE       TOK-F02 TO      M-MOTONM
     END-READ.
*    振替先（倉庫）名セット
     MOVE     IN-F19     TO      SOK-F01.
*
     READ     ZSOKMS
           INVALID       KEY
              MOVE       SPACE   TO      M-SAKINM
           NOT INVALID   KEY
              MOVE       SOK-F02 TO      M-SAKINM
     END-READ.
*
     WRITE    PRT-REC    FROM    MEISAI-1 AFTER  2.
     ADD      2          TO      L-CNT.
*
 MEISAI1-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    INFILE HTOKMS ZSOKMS HMEIMS PRINTF.
     DISPLAY "READ  ｹﾝｽｳ = " READ-CNT  UPON  CONS.
     DISPLAY "PAGE    ｽｳ = " P-CNT     UPON  CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
