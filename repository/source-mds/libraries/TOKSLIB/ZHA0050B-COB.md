# ZHA0050B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZHA0050B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　発注入力チェックリスト            *
*    作成日／更新日　　　：　93/05/12                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　発注入力チェックリストの出力を行う*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZHA0050B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YA-21     IS   YA-21
         YB        IS   YB
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 本日分発注ファイル   >>******************************
***  SELECT   ZHACHD    ASSIGN    TO        DA-01-S-ZHACHWK
***                     ACCESS    MODE      IS   SEQUENTIAL
****                    FILE      STATUS    IS   ZHA-STATUS.
***-<<  発注ファイル  >>---*
     SELECT   ZHACHD    ASSIGN    TO        DA-01-VI-ZHACHDT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   HAC-F01
                                                 HAC-F02
                                                 HAC-F03
                                                 HAC-F04
                                                 HAC-F05
                        FILE      STATUS    IS   ZHA-STATUS.
*
****<< 倉庫マスタファイル >>********************************
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
****<< 条件ファイル       >>********************************
     SELECT   JYOKEN    ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
****<< 取引先マスタ       >>********************************
     SELECT   TOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*
****<< 店舗マスタ         >>********************************
     SELECT   TENMS     ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*
****<< 仕入先マスタ       >>********************************
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZSH-F01
                        FILE      STATUS    IS   ZSH-STATUS.
*
****<< 商品名称マスタファイル >>****************************
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*
*
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 本日分発注ファイル   >>******************************
 FD  ZHACHD.
     COPY     ZHACHDT.
****<< 条件ファイル         >>******************************
 FD  JYOKEN.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<< 取引先マスタ         >>******************************
 FD  TOKMS.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
****<< 店舗マスタ           >>******************************
 FD    TENMS.
       COPY   TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
****<< 仕入先マスタ         >>******************************
 FD    ZSHIMS.
       COPY   ZSHIMS1   OF        XFDLIB
              JOINING   ZSH       PREFIX.
****<< 倉庫マスタファイル >>********************************
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
****<< 商品名称マスタファイル >>****************************
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZHA-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
     02 TOK-STATUS           PIC  X(2).
     02 TEN-STATUS           PIC  X(2).
     02 ZSH-STATUS           PIC  X(2).
 01  WK-SDATE                PIC  9(08)    VALUE ZERO.
 01  WK-DATE                 PIC  X(06)    VALUE SPACE.
 01  WK-F98                  PIC  9(08)    VALUE ZERO.
 01  WK-F99                  PIC  9(08)    VALUE ZERO.
*
****  見出し行１－１         ****
 01  MIDASI1        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(34)  VALUE  SPACE.
     02  FILLER              PIC  N(19)  VALUE
         NC"＊＊＊　発注入力チェックリスト　＊＊＊".
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  99.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZZ9.
****  見出し行２             ****
 01  MIDASI2        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"伝票区分".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注_".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注日".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"納　期".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"受担".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"場　所".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"量販_".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"送　料".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"税区".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"メモ".
****  見出し行３             ****
 01  MIDASI3        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"仕入先".
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"得意先".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"納入先".
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"送り先".
****  見出し行５             ****
 01  MIDASI5        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"行".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"商品".
     02  FILLER              PIC  X(04)  VALUE  "ｺ-ﾄﾞ".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"品単".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"商　品　名".
     02  FILLER              PIC  X(37)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"数　量".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"単".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"仕単価".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"単".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"原単価".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"販売単価".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"備　考".
****  明細行１               ****
 01  MEISAI1        CHARACTER     TYPE   IS   YB.
     02  PRT01               PIC  N(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT02               PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT03               PIC  N(05).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT04               PIC  9(09).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT05-YY            PIC  Z9.
     02  P1                  PIC  X(01).
     02  PRT05-MM            PIC  Z9.
     02  P2                  PIC  X(01).
     02  PRT05-DD            PIC  Z9.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT06-YY            PIC  Z9.
     02  P3                  PIC  X(01).
     02  PRT06-MM            PIC  Z9.
     02  P4                  PIC  X(01).
     02  PRT06-DD            PIC  Z9.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT07               PIC  X(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT08               PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT09               PIC  N(05).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT10               PIC  9(09).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  PRT11               PIC  9(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT12               PIC  ZZZ,ZZ9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  PRT13               PIC  9(01).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  PRT14               PIC  X(07).
****  明細行２               ****
 01  MEISAI2        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  PRT15               PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT16               PIC  N(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT17               PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT18               PIC  N(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT19               PIC  9(05).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT20               PIC  N(10).
****  明細行３               ****
 01  MEISAI3        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  PRT21               PIC  X(05).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT22               PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT23               PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT24               PIC  N(18).
****  明細行４               ****
 01  MEISAI4        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT25               PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT26               PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT27               PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT28               PIC  N(30).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT29               PIC  ZZZ,ZZ9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT30               PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT31               PIC  Z,ZZZ,ZZ9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT32               PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT33               PIC  Z,ZZZ,ZZ9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT34               PIC  ZZZ,ZZ9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT35               PIC  X(10).
****  テーブル                ****
 01  TBL-AREA.
     02  HZ-TBL          OCCURS   6.
         03  HZ-GYO      PIC  9(02).
         03  HZ-SYOCD    PIC  X(08).
         03  HZ-HINTAN   PIC  X(08).
         03  HZ-SURYO    PIC  9(08)V99.
         03  HZ-TAN24    PIC  9(01).
         03  HZ-SIRTAN   PIC  9(07)V99.
         03  HZ-TAN26    PIC  9(01).
         03  HZ-GENTAN   PIC  9(07)V99.
         03  HZ-HANTAN   PIC  9(07)V99.
         03  HZ-BIKO     PIC  X(10).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
 01  GYO-ZAN                 PIC  9(3).
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  SOK-FLG                 PIC  9(01)  VALUE  1.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
 01  INV-FLG                 PIC  9(01)  VALUE  ZERO.
****  インデックス            ****
 01  IXA                     PIC  9(02).
 01  IX                      PIC  9(02).
 01  X                       PIC  9(02).
 01  Z                       PIC  9(02).
****  ワークエリア            ****
 01  WORK-AREA.
     02  WORK-KEY1.
         03  WORK-NKEY       PIC  X(07).
         03  WORK-NCD        PIC  X(02).
     02  WORK-KEY2.
         03  WORK-OKEY       PIC  X(07).
         03  WORK-OCD        PIC  X(02).
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(6).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZHA0050B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZHACHD.
     MOVE   "ZHACHD  "        TO    ERR-FL-ID.
     MOVE    ZHA-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JYOKEN.
     MOVE   "JYOKEN  "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TOKMS.
     MOVE   "TOKMS   "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TENMS.
     MOVE   "TENMS   "        TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC7           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSHIMS.
     MOVE   "ZSHIMS  "        TO    ERR-FL-ID.
     MOVE    ZSH-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 ZHA0050B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 ZHA0050B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     ZHACHD   JYOKEN    TOKMS
                        TENMS    ZSHIMS    ZSOKMS    HMEIMS.
     OPEN     OUTPUT    PRINTF.
     MOVE     LOW-VALUE     TO        WORK-KEY1
                                      WORK-KEY2.
     MOVE     56            TO        L-CNT.
     MOVE     56            TO        GYO-ZAN.
     MOVE     1             TO        P-CNT.
     PERFORM                          ZHA-READ.
     ACCEPT   SYSYMD    FROM     DATE.
     ACCEPT   WK-SDATE  FROM     DATE.
*
**** MOVE     WK-SDATE(3:6) TO        WK-DATE.
     DISPLAY  WK-SDATE       UPON CONS.
     DISPLAY  WK-DATE       UPON CONS.
*
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     INITIALIZE                  TBL-AREA.
     MOVE    ZERO           TO   Z.
     MOVE    WORK-NKEY      TO   WORK-OKEY.
     MOVE    SPACE          TO   MEISAI1
                                 MEISAI2
                                 MEISAI3
                                 MEISAI4.
     PERFORM   UNTIL  (WORK-NKEY    NOT  =   WORK-OKEY)  OR
                      (WORK-NCD     <=       WORK-OCD)
        IF  HA0-F05    =    00
            MOVE  HA0-F07        TO   PRT21
            MOVE  HA0-F08        TO   PRT22
            MOVE  HA0-F09        TO   PRT23
            MOVE  HA0-F10        TO   PRT24
            MOVE  HA0-F01        TO   PRT02
        ELSE
            IF  HAC-F05     >=   01   AND
                HAC-F05     <=   06
                ADD    1         TO   Z
                IF     Z     =   1
                       PERFORM   PRT-KYOTU
                END-IF
                MOVE   HAC-F05   TO   HZ-GYO    (Z)
                MOVE   HAC-F20   TO   HZ-SYOCD  (Z)
                MOVE   HAC-F21   TO   HZ-HINTAN (Z)
                MOVE   HAC-F22   TO   HZ-SURYO  (Z)
                MOVE   HAC-F24   TO   HZ-TAN24  (Z)
                MOVE   HAC-F25   TO   HZ-SIRTAN (Z)
                MOVE   HAC-F26   TO   HZ-TAN26  (Z)
                MOVE   HAC-F27   TO   HZ-GENTAN (Z)
                MOVE   HAC-F28   TO   HZ-HANTAN (Z)
                MOVE   HAC-F29   TO   HZ-BIKO   (Z)
            END-IF
        END-IF
        MOVE    WORK-NCD         TO   WORK-OCD
        PERFORM                       ZHA-READ
     END-PERFORM.
     IF  P-CNT    =    01
         PERFORM       MIDASI-SEC
     END-IF.
     IF  GYO-ZAN   <   4    +   Z
         PERFORM       MIDASI-SEC
     END-IF.
     PERFORM           PRT-SEC.
 MAIN-END.
     EXIT.
************************************************************
*     2.1         プリント共通項目転送処理                 *
************************************************************
 PRT-KYOTU                  SECTION.
     IF  HAC-F37    =   ZERO
         IF  HAC-F99    =   ZERO
             MOVE      NC"登録"       TO   PRT01
         ELSE
             MOVE      NC"修正"       TO   PRT01
         END-IF
     ELSE
         MOVE     NC"削除"       TO   PRT01
     END-IF.
     MOVE    HAC-F01             TO   PRT02.
     PERFORM      JYO-READ.
     IF   INV-FLG   =  ZERO
          MOVE    JYO-F03        TO   PRT03
     END-IF.
     MOVE    HAC-F02             TO   PRT04(1:7).
     MOVE    HAC-F03             TO   PRT04(8:2).
     IF  HAC-F12  NOT   =   ZERO
         MOVE     HAC-F12(3:2)   TO   PRT05-YY
         MOVE     HAC-F12(5:2)   TO   PRT05-MM
         MOVE     HAC-F12(7:2)   TO   PRT05-DD
         MOVE     "."            TO   P1
                                      P2
     END-IF.
     IF  HAC-F13  NOT   =   ZERO
         MOVE     HAC-F13(3:2)   TO   PRT06-YY
         MOVE     HAC-F13(5:2)   TO   PRT06-MM
         MOVE     HAC-F13(7:2)   TO   PRT06-DD
         MOVE     "."            TO   P3
                                      P4
     END-IF.
     MOVE    HAC-F09             TO   PRT07.
     IF  HAC-F31  NOT =   ZERO
         MOVE    HAC-F31         TO   PRT08
         MOVE    1               TO   SOK-FLG
         PERFORM      SOK-READ
         IF  INV-FLG  =   ZERO
             MOVE     SOK-F02    TO   PRT09
         END-IF
     END-IF.
     IF  HAC-F07  NOT =   "000000000"
         MOVE     HAC-F07        TO   PRT10
     END-IF.
     MOVE    HAC-F16             TO   PRT11.
     MOVE    HAC-F17             TO   PRT12.
     MOVE    HAC-F15             TO   PRT13.
     MOVE    HAC-F18             TO   PRT14.
     IF   HAC-F08   NOT =  SPACE
          MOVE    HAC-F08        TO   PRT15
          PERFORM      ZSH-READ
          IF  INV-FLG  =  ZERO
              MOVE     ZSH-F02   TO   PRT16
          ELSE
              MOVE     SPACE     TO   PRT16
          END-IF
     END-IF.
     IF   HAC-F10   NOT =   "00000000"
          MOVE    HAC-F10             TO   PRT17
          PERFORM      TOK-READ
          IF  INV-FLG  =  ZERO
              MOVE     TOK-F02        TO   PRT18
          ELSE
              MOVE     SPACE          TO   PRT18
          END-IF
     END-IF.
     IF  HAC-F01  =   70  OR  71  OR  80  OR  81
         IF  HAC-F10(1:5)   =   99999
             IF  HAC-F41    NOT =    "00"
                 MOVE       HAC-F41   TO   PRT19
                 MOVE       2         TO   SOK-FLG
                 PERFORM    SOK-READ
                 IF  INV-FLG   =   ZERO
                     MOVE   SOK-F02   TO   PRT20
                 ELSE
                     MOVE   SPACE     TO   PRT20
                 END-IF
             END-IF
         ELSE
             IF  HAC-F19    NOT =     "00000"
                 MOVE     HAC-F19     TO   PRT19
                 PERFORM  TEN-READ
                 IF  INV-FLG  =  ZERO
                     MOVE   TEN-F02   TO   PRT20
                 ELSE
                     MOVE   SPACE     TO   PRT20
                 END-IF
             END-IF
         END-IF
     ELSE
         IF  HAC-F19   NOT =     "00000"
             MOVE      HAC-F19        TO   PRT19
             PERFORM   TEN-READ
             IF  INV-FLG  =  ZERO
                 MOVE  TEN-F02        TO   PRT20
             ELSE
                 MOVE  SPACE          TO   PRT20
             END-IF
         END-IF
     END-IF.
 PRT-KYOTU-END.
     EXIT.
************************************************************
*     2.1.1       伝票区分名の取得                         *
************************************************************
 JYO-READ                        SECTION.
     MOVE    01             TO   JYO-F01.
     MOVE    HAC-F01        TO   JYO-F02.
     READ    JYOKEN
       INVALID      KEY
          MOVE     "1"      TO   INV-FLG
       NOT INVALID  KEY
          MOVE      ZERO    TO   INV-FLG
     END-READ.
 JYO-READ-END.
     EXIT.
************************************************************
*    2.1.2        倉庫名の取得                             *
************************************************************
 SOK-READ                        SECTION.
     IF  SOK-FLG  =   1
         MOVE     HAC-F31        TO   SOK-F01
     ELSE
         MOVE     HAC-F41        TO   SOK-F01
     END-IF.
     READ    ZSOKMS
       INVALID      KEY
          MOVE     "1"      TO   INV-FLG
       NOT INVALID  KEY
          MOVE      ZERO    TO   INV-FLG
     END-READ.
 SOK-READ-END.
     EXIT.
************************************************************
*    2.1.3        仕入先名の取得                           *
************************************************************
 ZSH-READ                        SECTION.
     MOVE    HAC-F08         TO  ZSH-F01.
     READ    ZSHIMS
       INVALID      KEY
          MOVE     "1"       TO  INV-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO  INV-FLG
     END-READ.
 ZSH-READ-END.
     EXIT.
************************************************************
*    2.1.4        得意先名の取得                           *
************************************************************
 TOK-READ                        SECTION.
     MOVE    HAC-F10         TO  TOK-F01.
     READ    TOKMS
       INVALID      KEY
          MOVE     "1"       TO  INV-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO  INV-FLG
     END-READ.
 TOK-READ-END.
     EXIT.
************************************************************
*    2.1.5        納入先名の取得                           *
************************************************************
 TEN-READ                        SECTION.
     MOVE    HAC-F10         TO  TEN-F52.
     MOVE    HAC-F19         TO  TEN-F011.
     READ    TENMS
       INVALID      KEY
          MOVE     "1"       TO  INV-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO  INV-FLG
     END-READ.
 TOK-READ-END.
     EXIT.
************************************************************
*      2.2       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE    SYS-YY     TO      H-YY.
     MOVE    SYS-MM     TO      H-MM.
     MOVE    SYS-DD     TO      H-DD.
     MOVE    P-CNT      TO      PAGE-SUU.
     IF      P-CNT    NOT =   1
             MOVE     SPACE   TO      P-REC
             WRITE    P-REC   AFTER   PAGE
             MOVE     56      TO      GYO-ZAN
     END-IF.
     WRITE   P-REC     FROM    MIDASI1     AFTER  2.
     WRITE   P-REC     FROM    MIDASI2     AFTER  2.
     WRITE   P-REC     FROM    MIDASI3     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
     WRITE   P-REC     FROM    MIDASI5     AFTER  1.
     ADD     1         TO      P-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*    2.3          プリント処理
************************************************************
 PRT-SEC                    SECTION.
     WRITE    P-REC         FROM    MEISAI1    AFTER  2.
     WRITE    P-REC         FROM    MEISAI2    AFTER  1.
     WRITE    P-REC         FROM    MEISAI3    AFTER  1.
     PERFORM  VARYING   X   FROM  1  BY  1
       UNTIL  (X    >    6)   OR   (X    >    Z)
        MOVE      HZ-GYO(X)      TO   PRT25
        MOVE      HZ-SYOCD(X)    TO   PRT26
        MOVE      HZ-HINTAN(X)   TO   PRT27
        PERFORM   MEI-READ
        IF  INV-FLG   =   ZERO
            MOVE  MEI-F021       TO   PRT28(1:15)
            MOVE  MEI-F022       TO   PRT28(16:15)
        END-IF
        MOVE      HZ-SURYO(X)    TO   PRT29
        MOVE      HZ-TAN24(X)    TO   PRT30
        MOVE      HZ-SIRTAN(X)   TO   PRT31
        MOVE      HZ-TAN26(X)    TO   PRT32
        MOVE      HZ-GENTAN(X)   TO   PRT33
        MOVE      HZ-HANTAN(X)   TO   PRT34
        MOVE      HZ-BIKO(X)     TO   PRT35
        WRITE     P-REC          FROM    MEISAI4  AFTER  1
     END-PERFORM.
     COMPUTE      GYO-ZAN    =   GYO-ZAN  -  (4   +   X   -  1).
     MOVE         LOW-VALUE      TO   WORK-OCD.
*
 PRT-SEC-END.
     EXIT.
************************************************************
*    2.3.1        商品名の取得                             *
************************************************************
 MEI-READ                        SECTION.
     MOVE    HZ-SYOCD(X)    TO   MEI-F011.
     MOVE    HZ-HINTAN(X)   TO   MEI-F012.
     READ    HMEIMS
       INVALID      KEY
          MOVE     "1"      TO   INV-FLG
       NOT INVALID  KEY
          MOVE      ZERO    TO   INV-FLG
     END-READ.
 MEI-READ-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE              PRINTF   ZHACHD    JYOKEN    TOKMS
                        TENMS    ZSHIMS    ZSOKMS    HMEIMS.
 END-END.
     EXIT.
************************************************************
*      本日分発注ファイルの読込み                          *
************************************************************
 ZHA-READ               SECTION.
     READ     ZHACHD
              AT  END
                  MOVE     "END"           TO  END-FLG
                  MOVE      HIGH-VALUE     TO  WORK-KEY1
                  GO        TO             ZHA-READ-EXIT
              NOT AT  END
                  MOVE      HAC-F98   TO   WK-F98
                  MOVE      HAC-F99   TO   WK-F99
*
                  IF        WK-F98(3:6) =  WK-SDATE(3:6)
********                   (HAC-F37   NOT  =  ZERO     )
********          DISPLAY                  WK-F98 UPON CONS
********          DISPLAY                  WK-F99 UPON CONS
                      CONTINUE
                  ELSE
                      GO    TO             ZHA-READ
                  END-IF
*
     END-READ.
     IF  HA0-F05  =    80   OR   99
         GO       TO   ZHA-READ
     END-IF.
     MOVE    HAC-F02        TO        WORK-NKEY.
     MOVE    HAC-F05        TO        WORK-NCD.
 ZHA-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
