# ZHA0110B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZHA0110B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　発注消込みチェックリスト          *
*    作成日／更新日　　　：　93/05/10                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　リスト出力を行う　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZHA0110B.
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
         YB        IS   YB1
         YB-21     IS   YB2
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入庫ファイル         >>******************************
     SELECT   ZNYUKDT   ASSIGN  TO   DA-01-VI-ZNYUKDT1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   NYUK-F01
                                                  NYUK-F02
                                                  NYUK-F03
                                                  NYUK-F04
                                                  NYUK-F05
                        FILE    STATUS       IS   NYUK-STATUS.
****<< 発注 ファイル         >>******************************
     SELECT   ZHACHDT   ASSIGN  TO   DA-01-VI-ZHACHDT1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   HAC-F01
                                                  HAC-F02
                                                  HAC-F03
                                                  HAC-F04
                                                  HAC-F05
                        FILE    STATUS       IS   HAC-STATUS.
****<< 店舗マスタ           >>******************************
     SELECT   HTENMS    ASSIGN  TO   DA-01-VI-TENMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TEN-F52
                                                  TEN-F011
                        FILE    STATUS       IS   TEN-STATUS.
****<< 商品名称マスタ       >>******************************
     SELECT   HMEIMS    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-F011
                                                  MEI-F012
                        FILE    STATUS       IS   MEI-STATUS.
****<< 仕入先マスタ         >>******************************
     SELECT   ZSHIMS    ASSIGN  TO   DA-01-VI-ZSHIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SHI-F01
                        FILE    STATUS       IS   SHI-STATUS.
****<< 取引先マスタ         >>******************************
     SELECT   HTOKMS    ASSIGN  TO   DA-01-VI-TOKMS2
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TOK-F01
                        FILE    STATUS       IS   TOK-STATUS.
****<< 倉庫マスタ           >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-STATUS.
****<< 条件ファイル         >>******************************
     SELECT   HJYOKEN   ASSIGN  TO   DA-01-VI-JYOKEN1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   JYO-F01
                                                  JYO-F02
                        FILE    STATUS       IS   JYO-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
****<<  画面ファイル        >>******************************
     SELECT   DSPF      ASSIGN  TO   GS-DSPF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        SYMBOLIC DESTINATION IS  "DSP"
                        PROCESSING MODE      IS   DSP-PROC
                        GROUP                IS   DSP-GROUP
                        FORMAT               IS   DSP-FORMAT
                        SELECTED FUNCTION    IS   DSP-FUNC
                        FILE STATUS          IS   DSP-STATUS.
 DATA                   DIVISION.
 FILE                   SECTION.
****<< 入庫ファイル         >>******************************
 FD    ZNYUKDT
       LABEL     RECORD    IS    STANDARD.
       COPY      ZNYUKDT   OF    XFDLIB
                 JOINING   NYUK  PREFIX.
****<< 発注ファイル         >>******************************
 FD    ZHACHDT
       LABEL     RECORD    IS    STANDARD.
       COPY      ZHACHDT.
****<< 店舗マスタ           >>******************************
 FD    HTENMS
       LABEL     RECORD    IS    STANDARD.
       COPY      HTENMS    OF    XFDLIB
                 JOINING   TEN   PREFIX.
****<< 商品名称マスタ       >>******************************
 FD    HMEIMS
       LABEL     RECORD    IS    STANDARD.
       COPY      HMEIMS    OF    XFDLIB
                 JOINING   MEI   PREFIX.
****<< 仕入先マスタ         >>******************************
 FD    ZSHIMS
       LABEL     RECORD    IS    STANDARD.
       COPY      ZSHIMS    OF    XFDLIB
                 JOINING   SHI   PREFIX.
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
****<< 条件ファイル         >>******************************
 FD    HJYOKEN
       LABEL     RECORD    IS    STANDARD.
       COPY      HJYOKEN   OF    XFDLIB
                 JOINING   JYO   PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     ZHA0110           OF   XMDLIB.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(02).
     02 DSP-GROUP            PIC  X(08).
     02 DSP-FORMAT           PIC  X(08).
     02 DSP-STATUS           PIC  X(02).
     02 DSP-FUNC             PIC  X(04).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 NYUK-STATUS          PIC  X(02).
     02 HAC-STATUS           PIC  X(02).
     02 TEN-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 SHI-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
****  フラグ                  ****
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE  SPACE.
     02  CURSOR-FLG          PIC  9(01)  VALUE  ZERO.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(06).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  日付チェック            ****
 01  CHK-HIZUKE.
     02  CHK-YY              PIC  9(04)  VALUE  ZERO.
     02  CHK-MM              PIC  9(02)  VALUE  ZERO.
     02  CHK-DD              PIC  9(02)  VALUE  ZERO.
****  カウンタ                ****
 01  CNT-AREA.
     02  L-CNT               PIC  9(02)  VALUE  90.
     02  P-CNT               PIC  9(07)  VALUE  ZERO.
     02  READ-CNT            PIC  9(07)  VALUE  ZERO.
     02  SELECT-CNT          PIC  9(07)  VALUE  ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
     02  TBL-CNT             PIC  9(01)  VALUE  ZERO.
     02  PRINT-CNT           PIC  9(02)  VALUE  ZERO.
****  ワーク                  ****
 01  WORK-AREA.
     02  WK-JYO-F04          PIC  9(02).
     02  WK-JYO-SOKCD
                  REDEFINES  WK-JYO-F04    PIC  X(02).
     02  WK-SOKNM            PIC  N(18)  VALUE  SPACE.
     02  WK-MATUBI           PIC  9(02)  VALUE  ZERO.
     02  WK-HACHZAN          PIC S9(08)V99      VALUE  ZERO.
     02  WK-HAC-F22          PIC S9(08)V99      VALUE  ZERO.
     02  WK-L-CNT            PIC  9(02)         VALUE  ZERO.
     02  WK-NYUK-KEY                     VALUE  ZERO.
       03  WK-NYUK-F01       PIC  9(02).
       03  WK-NYUK-F02       PIC  9(07).
       03  WK-NYUK-F03       PIC  9(02).
       03  WK-NYUK-F04       PIC  9(01).
     02  WK-HIKAKU-KEY       PIC  9(12).
****  インデックス            ****
 01  IX                      PIC  9(01).
****  入庫ファイルテーブル    ****
 01  TBL-NYUKF.
     02  TBL-NYUK        OCCURS  6.
       03  TBL-NYUK-F01      PIC  9(02).
       03  TBL-NYUK-F02      PIC  9(07).
       03  TBL-NYUK-F03      PIC  9(02).
       03  TBL-NYUK-F04      PIC  9(01).
       03  TBL-NYUK-F05      PIC  9(02).
       03  TBL-NYUK-F06      PIC  9(09).
       03  TBL-NYUK-F07      PIC  X(08).
       03  TBL-NYUK-F08      PIC  9(08).
       03  TBL-NYUK-F09      PIC  9(01).
       03  TBL-NYUK-F10      PIC  9(01).
       03  TBL-NYUK-F11      PIC  9(07)        PACKED-DECIMAL.
       03  TBL-NYUK-F12      PIC  9(05).
       03  TBL-NYUK-F13      PIC  X(08).
       03  TBL-NYUK-F14      PIC  X(08).
       03  TBL-NYUK-F15      PIC  9(08)V9(02)  PACKED-DECIMAL.
       03  TBL-NYUK-F16      PIC  9(01).
       03  TBL-NYUK-F17      PIC  9(07)V9(02)  PACKED-DECIMAL.
       03  TBL-NYUK-F18      PIC  9(01).
       03  TBL-NYUK-F19      PIC  9(07)V9(02)  PACKED-DECIMAL.
       03  TBL-NYUK-F20      PIC  9(07)V9(02)  PACKED-DECIMAL.
       03  TBL-NYUK-F21      PIC  X(10).
       03  TBL-NYUK-F22      PIC  X(06).
       03  TBL-NYUK-F23      PIC  9(08).
       03  TBL-NYUK-F24      PIC  X(02).
       03  TBL-NYUK-F25      PIC  9(01).
       03  TBL-NYUK-F26      PIC  9(01).
       03  TBL-NYUK-F27      PIC  9(01).
       03  TBL-NYUK-F28      PIC  9(01).
       03  TBL-NYUK-F29      PIC  9(08).
       03  TBL-NYUK-F30      PIC  X(02).
       03  TBL-NYUK-F31      PIC  9(08)V9(02)  PACKED-DECIMAL.
       03  FILEER            PIC  X(48)  VALUE  SPACE.
       03  TBL-NYUK-F91      PIC  X(01).
       03  TBL-NYUK-F98      PIC  9(08).
       03  TBL-NYUK-F99      PIC  9(08).
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  M-KEIJYO            PIC  N(06)
                             CHARACTER   TYPE   IS   YB2.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(19)  VALUE
         NC"＊＊＊　発注消込チェックリスト　＊＊＊"
                             CHARACTER   TYPE   IS   YB2.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"頁"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-PAGE              PIC  ZZZ9   VALUE  ZERO.
     02  FILLER              PIC  X(11)  VALUE  SPACE.
****  見出し行２             ****
 01  MIDASI-2      CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"伝票区分".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"相殺".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"納品日".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"発注_".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"発注日".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"場所".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"量販_".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"税区".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"メモ".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"入力日".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  H2-NYURYOKU1        PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  H2-NYURYOKU2        PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  H2-NYURYOKU3        PIC  Z9.
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"仕入先".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"得意先".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"納入先".
     02  FILLER              PIC  X(74)  VALUE  SPACE.
****  見出し行４             ****
 01  MIDASI-4       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"行".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"完".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"商品".
     02  FILLER              PIC  X(04)  VALUE  "ｺｰﾄﾞ".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"品　単".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"商品名".
     02  FILLER              PIC  X(43)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"発注数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"入荷数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"発注残".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"備　考".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"_　番".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
****  明細行１               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  M1-DENKB            PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M1-DENKBNM          PIC  N(05)
                             CHARACTER     TYPE   IS   YB1.
     02  FILLER              PIC  N(01)  VALUE  SPACE
                             CHARACTER     TYPE   IS   YB1.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M1-SOUSAI           PIC  9(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M1-NOHINYY          PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  M1-NOHINMM          PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  M1-NOHINDD          PIC  Z9.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M1-HACHUNO          PIC  9(07).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M1-EDABAN           PIC  9(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M1-HACHUYY          PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  M1-HACHUMM          PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  M1-HACHUDD          PIC  Z9.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M1-BASYO            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M1-SOKONM           PIC  N(05)
                             CHARACTER     TYPE   IS   YB1.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M1-RYOHAN           PIC  9(09).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  M1-ZEIKB            PIC  9(01).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  M1-MEMO             PIC  X(07).
     02  FILLER              PIC  X(46)  VALUE  SPACE.
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB1.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  M2-SHIRECD          PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M2-SHIRENM          PIC  N(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M2-TOKUICD          PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M2-TOKUINM          PIC  N(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M2-NOUNYU.
         03  M2-NONYUCD      PIC  9(05).
         03  FILLER          PIC  X(01)  VALUE  SPACE.
         03  M2-NONYUNM      PIC  N(10).
     02  FILLER              PIC  X(59)  VALUE  SPACE.
****  明細行３               ****
 01  MEISAI-3       CHARACTER     TYPE   IS   YB1.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  M3-GYONO            PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M3-KANRYO           PIC  X(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M3-SHOCD            PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M3-HINTAN           PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M3-SHONM            PIC  N(30).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M3-HACHUSU          PIC  Z,ZZZ,ZZ9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M3-NYUKOSU          PIC  Z,ZZZ,ZZ9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M3-HACHUZAN         PIC  -,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M3-BIKOU            PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M3-TANABAN1         PIC  X(01).
     02  M3-FILLER1          PIC  X(01).
     02  M3-TANABAN2         PIC  X(03).
     02  M3-FILLER2          PIC  X(01).
     02  M3-TANABAN3         PIC  X(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01              PIC N(20) VALUE
                             NC"_終了".
     02  PMSG02              PIC N(20) VALUE
                             NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZHA0110B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-CD              PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ        ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(20)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"倉庫コードを入力して下さい。".
     02  MSG-ERR3            PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     02  MSG-ERR4            PIC  N(20)  VALUE
            NC"指定の倉庫コードは扱えません。".
     02  MSG-ERR5            PIC  N(20)  VALUE
            NC"対象年月日に誤りがあります。".
     02  MSG-ERR6            PIC  N(20)  VALUE
            NC"計上区分に誤りがあります。".
     02  MSG-ERR7            PIC  N(20)  VALUE
            NC"計上区分は入力できません。".
 01  ERR-MSG-ALL             REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS   7  TIMES.
****  エラーメッセージ　ＦＯＲ　コンソール  ***
 01  FILE-ERROR.
     02  FILE-ERR1           PIC  N(10)  VALUE
            NC"入庫ファイル　異常！".
     02  FILE-ERR2           PIC  N(10)  VALUE
            NC"発注ファイル　異常！".
     02  FILE-ERR3           PIC  N(10)  VALUE
            NC"店舗マスタ　　異常！".
     02  FILE-ERR4           PIC  N(10)  VALUE
            NC"商品名称マスタ異常！".
     02  FILE-ERR5           PIC  N(10)  VALUE
            NC"仕入先マスタ　異常！".
     02  FILE-ERR6           PIC  N(10)  VALUE
            NC"取引先マスタ　異常！".
     02  FILE-ERR7           PIC  N(10)  VALUE
            NC"倉庫マスタ　　異常！".
     02  FILE-ERR8           PIC  N(10)  VALUE
            NC"条件ファイル　異常！".
     02  FILE-ERR9           PIC  N(10)  VALUE
            NC"画面ファイル　異常！".
**** パラメータ               ****
 LINKAGE                SECTION.
 01  PARA-AREA.
     02    PARA-WKSTN        PIC  X(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION      USING   PARA-AREA.
*
 DECLARATIVES.
**入庫ファイル
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZNYUKDT.
     MOVE   "ZNYUKDT "        TO    ERR-FL-ID.
     MOVE    NYUK-STATUS      TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**発注ファイル
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZHACHDT.
     MOVE   "ZHACHDT "        TO    ERR-FL-ID.
     MOVE    HAC-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**店舗マスタ
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTENMS.
     MOVE   "HTENMS "         TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
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
**取引先マスタ
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTOKMS.
     MOVE   "HTOKMS  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**倉庫マスタ
 FILEERR-SEC7           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**条件ファイル
 FILEERR-SEC8           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**画面ファイル
 FILEERR-SEC9           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 ZHA0110B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     WK-NYUK-KEY  =    HIGH-VALUE
                       OR    END-FLG      =   "END".
     PERFORM       END-SEC.
     STOP     RUN.
 ZHA0110B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     ZNYUKDT.
     OPEN     INPUT     ZHACHDT.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     ZSHIMS.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HJYOKEN.
     OPEN     OUTPUT    PRINTF.
*---- システム日付　取得
     ACCEPT   SYSYMD    FROM   DATE.
     MOVE     SYS-YY    TO     H-YY.
     MOVE     SYS-MM    TO     H-MM.
     MOVE     SYS-DD    TO     H-DD.
*---- 条件ファイル　索引
     MOVE     65          TO     JYO-F01.
     MOVE     PARA-WKSTN  TO     JYO-F02.
     READ     HJYOKEN
         INVALID
           DISPLAY  "JYOKEN INV-KEY  = " JYO-F01  UPON  CONS
           DISPLAY  "JYOKEN INV-KEY  = " JYO-F02  UPON  CONS
           MOVE     "END"       TO     END-FLG
           GO                   TO     INIT-SEC-EXIT
         NOT  INVALID
           MOVE     JYO-F04     TO     WK-JYO-F04
     END-READ.
*
     PERFORM     WRITE-SELECT-SEC.
     IF    END-FLG  NOT =   "END"
        PERFORM     ZNYUKDT-READ-SEC
     END-IF.
 INIT-SEC-EXIT.
     EXIT.
****************************************************************
*      _１　　出力条件指定　処理                              *
****************************************************************
 WRITE-SELECT-SEC            SECTION.
*----- 初画面出力 ------*
     PERFORM    DSP-INIT-SEC.
*----- 画面ＲＥＡＤ ----*
 WS010.
     MOVE    "BODY"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　_１ ----*
     MOVE    "M"                 TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SOKCD.
     MOVE    "M"                 TO   EDIT-OPTION   OF   YY.
     MOVE    " "                 TO   EDIT-CURSOR   OF   YY.
     MOVE    "M"                 TO   EDIT-OPTION   OF   MM.
     MOVE    " "                 TO   EDIT-CURSOR   OF   MM.
     MOVE    "M"                 TO   EDIT-OPTION   OF   DD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   DD.
     EVALUATE      DSP-FUNC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             GO                  TO   WRITE-SELECT-END
         WHEN     "E000"
             CONTINUE
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             MOVE   "R"          TO   EDIT-OPTION   OF   SOKCD
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS010
     END-EVALUATE.
*----- 入力項目チェック---*
*（入力有無チェック）*
     IF   SOKCD   =   SPACE
         MOVE    2               TO   ERR-CD
         MOVE   "R"              TO   EDIT-OPTION   OF   SOKCD
     END-IF.
*
*（存在チェック）*
     MOVE    SOKCD               TO   SOK-F01.
     READ    ZSOKMS
         INVALID KEY
             MOVE    SPACE       TO   SOKNM
             MOVE    "R"         TO   EDIT-OPTION   OF   SOKCD
             IF   ERR-CD    =    ZERO
                MOVE    3        TO   ERR-CD
             END-IF
         NOT INVALID KEY
             MOVE    SOK-F02     TO   SOKNM
             MOVE    SOK-F02     TO   WK-SOKNM
     END-READ.
*
*\\  93.06.07  START \\\
     IF  KEIJYO      NOT =       ZERO AND 1 AND 9
*
         MOVE    "R"         TO   EDIT-OPTION   OF   KEIJYO
         IF       ERR-CD    =    ZERO
                  MOVE    6      TO   ERR-CD
         END-IF
     END-IF.
* （条件ファイルでの存在チェック）*
     IF  WK-JYO-SOKCD   NOT =   "01"
         IF  SOKCD      NOT =   WK-JYO-SOKCD
             MOVE   "R" TO      EDIT-OPTION   OF   SOKCD
             IF   ERR-CD    =   ZERO
                  MOVE    4        TO   ERR-CD
             END-IF
         END-IF
     END-IF.
     IF  WK-JYO-SOKCD   NOT =   "01"
         IF  KEIJYO     NOT =   ZERO
             MOVE   "R" TO      EDIT-OPTION   OF   KEIJYO
             IF   ERR-CD    =   ZERO
                  MOVE    7        TO   ERR-CD
             END-IF
         END-IF
     END-IF.
*
*（対象年月日チェック）*
     IF    YY    >=  93  AND   YY   <=  99
       THEN
           COMPUTE   CHK-YY  =  YY  + 1900
       ELSE
           COMPUTE   CHK-YY  =  YY  + 2000
     END-IF.
     MOVE      MM            TO    CHK-MM.
     MOVE      DD            TO    CHK-DD.
     IF    MM    >=  1    AND   MM   <=  12
       THEN
           CONTINUE
       ELSE
           MOVE      "R"     TO    EDIT-OPTION   OF   MM
           IF  ERR-CD    =    ZERO
               MOVE   5      TO    ERR-CD
               MOVE   1      TO    CURSOR-FLG
           END-IF
     END-IF.
     EVALUATE    MM
       WHEN  2
             MOVE    29      TO    WK-MATUBI
       WHEN  4
       WHEN  6
       WHEN  9
       WHEN  11
             MOVE    30      TO    WK-MATUBI
       WHEN  OTHER
             MOVE    31      TO    WK-MATUBI
     END-EVALUATE.
     IF    DD    >=  1    AND   DD   <=  WK-MATUBI
         CONTINUE
       ELSE
         MOVE    "R"         TO    EDIT-OPTION   OF   DD
         IF   ERR-CD    =    ZERO
           MOVE       5      TO    ERR-CD
           MOVE       2      TO    CURSOR-FLG
         END-IF
     END-IF.
*----- エラーが有った時 ----*
     IF   ERR-CD  NOT =   ZERO
       EVALUATE   ERR-CD
         WHEN  1
         WHEN  2
         WHEN  3
         WHEN  4
           MOVE    "C"       TO   EDIT-CURSOR   OF   SOKCD
         WHEN  5
           EVALUATE   CURSOR-FLG
             WHEN  1
               MOVE    "C"   TO   EDIT-CURSOR   OF   MM
             WHEN  2
               MOVE    "C"   TO   EDIT-CURSOR   OF   DD
           END-EVALUATE
       END-EVALUATE
       MOVE    "BODY"        TO   DSP-GROUP
       PERFORM    DSP-ERR-SEC
       GO                    TO   WS010
     END-IF.
*----- ボディー部の出力 ----*
     MOVE    PMSG02          TO   PFKGID.
     MOVE    "M"             TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "             TO   EDIT-CURSOR   OF   SOKCD.
     MOVE    "M"             TO   EDIT-OPTION   OF   YY.
     MOVE    " "             TO   EDIT-CURSOR   OF   YY.
     MOVE    "M"             TO   EDIT-OPTION   OF   MM.
     MOVE    " "             TO   EDIT-CURSOR   OF   MM.
     MOVE    "M"             TO   EDIT-OPTION   OF   DD.
     MOVE    " "             TO   EDIT-CURSOR   OF   DD.
     MOVE    "BODY"          TO   DSP-GROUP.
     MOVE    SPACE           TO   ERRMSG.
     PERFORM    DSP-WRITE-SEC.
*----- 確認画面の出力-----*
     MOVE    "TAIL"              TO   DSP-GROUP.
     PERFORM    DSP-WRITE-SEC.
*----- 確認画面の入力-----*
 WS020.
     MOVE    "TAIL"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　_２ ----*
     EVALUATE      DSP-FUNC
         WHEN     "F004"
             GO                  TO   WRITE-SELECT-SEC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             GO                  TO   WRITE-SELECT-END
         WHEN     "E000"
             CONTINUE
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             MOVE    "TAIL"      TO   DSP-GROUP
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS020
     END-EVALUATE.
 WRITE-SELECT-END.
     EXIT.
****************************************************************
*      __１　初期画面　出力　処理                            *
****************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE    SPACE               TO   ZHA0110.
     MOVE    SPACE               TO   DSP-CONTROL.
     MOVE    "CL"                TO   DSP-PROC.
     MOVE    "ZHA0110"           TO   DSP-FORMAT.
     MOVE    "ALL"               TO   DSP-GROUP.
     MOVE    SYS-YY              TO   YY.
     MOVE    SYS-MM              TO   MM.
     MOVE    SYS-DD              TO   DD.
     MOVE    PMSG01              TO   PFKGID.
*****DISPLAY "WK-JYO-F04 = " WK-JYO-F04 UPON CONS.
     IF      WK-JYO-F04     NOT =     1
         MOVE    WK-JYO-F04      TO   SOK-F01
         READ    ZSOKMS
            INVALID KEY
               MOVE    SPACE       TO   SOKNM
               MOVE    "R"         TO   EDIT-OPTION   OF   SOKCD
             NOT INVALID KEY
               MOVE    WK-JYO-F04  TO   SOKCD
               MOVE    SOK-F02     TO   SOKNM
               MOVE    SOK-F02     TO   WK-SOKNM
         END-READ
     END-IF.
     WRITE                  ZHA0110.
     INITIALIZE             ZHA0110.
*
 DSP-INIT-END.
     EXIT.
****************************************************************
*      __２　画面ＲＥＡＤ　処理                              *
****************************************************************
 DSP-READ-SEC           SECTION.
     MOVE    "NE"                TO   DSP-PROC.
     READ    DSPF
         AT   END
             GO                  TO   DSP-READ-END
     END-READ.
     IF   DSP-STATUS   NOT =   ZERO
         DISPLAY   FILE-ERR9   UPON   CONS
     END-IF.
 DSP-READ-END.
     EXIT.
****************************************************************
*      __３　画面ＷＲＩＴＥ　処理                            *
****************************************************************
 DSP-WRITE-SEC          SECTION.
     MOVE    SPACE               TO   DSP-PROC.
     WRITE                  ZHA0110.
 DSP-WRITE-END.
     EXIT.
****************************************************************
*      1.1.4     エラーメッセージセット　処理                  *
****************************************************************
 DSP-ERR-SEC                 SECTION.
*---- エラー メッセージ セット ----*
     MOVE    ERR-MSG(ERR-CD)     TO   ERRMSG.
     MOVE    ZERO                TO   ERR-CD.
     MOVE    ZERO                TO   CURSOR-FLG.
     PERFORM    DSP-WRITE-SEC.
 DSP-ERR-END.
     EXIT.
************************************************************
*      1.2       入庫ファイル　ＲＥＡＤ処理                *
************************************************************
 ZNYUKDT-READ-SEC       SECTION.
     READ    ZNYUKDT
       AT  END
           MOVE     HIGH-VALUE   TO   WK-NYUK-KEY
           GO       TO   ZNYUKDT-READ-EXIT
     END-READ.
     ADD      1          TO   READ-CNT.
*\\  93.06.07  START \\\
     IF       WK-JYO-F04 NOT =   "01"
         IF   NYUK-F01   NOT =   "50"
         GO              TO   ZNYUKDT-READ-SEC
         END-IF
     END-IF.
*
     IF       KEIJYO     =       1
        IF    NYUK-F27   =       ZERO
         GO              TO   ZNYUKDT-READ-SEC
        END-IF
     END-IF.
*****IF       KEIJYO     =       9
*****   IF    NYUK-F27   NOT =   ZERO
*****    GO              TO   ZNYUKDT-READ-SEC
*****   END-IF
*****END-IF.
     IF      (KEIJYO     =       9)        AND
             (WK-JYO-SOKCD  NOT =     "01")
        IF    NYUK-F27   NOT =   ZERO
         GO              TO   ZNYUKDT-READ-SEC
        END-IF
     END-IF.
*\\  93.06.07  END   \\\
*
     IF       WK-JYO-SOKCD  NOT =     "01"
         IF   NYUK-F01      NOT =      50
              GO         TO   ZNYUKDT-READ-SEC
         END-IF
*
        IF    NYUK-F27   NOT =   ZERO
              GO         TO   ZNYUKDT-READ-SEC
        END-IF
     END-IF.
*
*****93/11/02
     IF  KEIJYO  =  9
         IF SOKCD   =    NYUK-F24      AND
            CHK-HIZUKE   =    NYUK-F98 AND
            NYUK-F01     =    50
                ADD      1             TO   SELECT-CNT
                MOVE     NYUK-F01      TO   WK-NYUK-F01
                MOVE     NYUK-F02      TO   WK-NYUK-F02
                MOVE     NYUK-F03      TO   WK-NYUK-F03
                MOVE     NYUK-F04      TO   WK-NYUK-F04
                GO       TO                 ZNYUKDT-READ-EXIT
            ELSE
                ADD  1          TO   SKIP-CNT
                GO              TO   ZNYUKDT-READ-SEC
         END-IF
     END-IF.
*
*
     IF  SOKCD   =  NYUK-F24  AND
         ( CHK-HIZUKE = NYUK-F98  OR  CHK-HIZUKE = NYUK-F99 )
       THEN
           ADD      1             TO   SELECT-CNT
           MOVE     NYUK-F01      TO   WK-NYUK-F01
           MOVE     NYUK-F02      TO   WK-NYUK-F02
           MOVE     NYUK-F03      TO   WK-NYUK-F03
           MOVE     NYUK-F04      TO   WK-NYUK-F04
       ELSE
         ADD  1          TO   SKIP-CNT
         GO              TO   ZNYUKDT-READ-SEC
     END-IF.
 ZNYUKDT-READ-EXIT.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     MOVE     WK-NYUK-KEY         TO   WK-HIKAKU-KEY.
     PERFORM  TBL-SET-SEC     VARYING  IX FROM   1 BY 1
              UNTIL   IX  >   6
                OR    WK-NYUK-KEY   NOT =  WK-HIKAKU-KEY.
     PERFORM  HENSYU-SEC.
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      _1       テーブル格納処理                          *
************************************************************
 TBL-SET-SEC            SECTION.
     MOVE     NYUK-REC      TO       TBL-NYUK(IX).
     ADD      1             TO       TBL-CNT.
     PERFORM  ZNYUKDT-READ-SEC.
  TBL-SET-EXIT.
************************************************************
*      _２      編集処理                                  *
************************************************************
 HENSYU-SEC             SECTION.
     EVALUATE     TBL-CNT
         WHEN  1
             MOVE     4     TO        PRINT-CNT
         WHEN  2
             MOVE     5     TO        PRINT-CNT
         WHEN  3
             MOVE     6     TO        PRINT-CNT
         WHEN  4
             MOVE     7     TO        PRINT-CNT
         WHEN  5
             MOVE     8     TO        PRINT-CNT
         WHEN  6
             MOVE     9     TO        PRINT-CNT
     END-EVALUATE.
*
     COMPUTE  WK-L-CNT  =  L-CNT   +   PRINT-CNT
     IF       WK-L-CNT     >       62
         PERFORM       MIDASI-SEC
     END-IF.
*
     PERFORM   PRINT-SEC    VARYING   IX  FROM  1  BY  1
               UNTIL        IX   >    TBL-CNT.
*
     MOVE     ZERO          TO        TBL-CNT.
 HNSYU-SEC-EXIT.
     EXIT.
************************************************************
*      2._1       見出し処理                              *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE         SPACE   TO      P-REC.
     INITIALIZE                   P-REC.
     ADD          1       TO      P-CNT.
     MOVE         P-CNT   TO      H-PAGE.
     MOVE         CHK-HIZUKE(3:2) TO   H2-NYURYOKU1.
     MOVE         CHK-HIZUKE(5:2) TO   H2-NYURYOKU2.
     MOVE         CHK-HIZUKE(7:2) TO   H2-NYURYOKU3.
     IF       P-CNT  =   1
          IF  KEIJYO =   1
              MOVE   NC"計　上　済"      TO         M-KEIJYO
          ELSE
              MOVE   SPACE               TO         M-KEIJYO
          END-IF
              WRITE      P-REC   FROM    MIDASI-1   AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
        ELSE
              WRITE      P-REC   AFTER   PAGE
          IF  KEIJYO =   1
              MOVE   NC"計　上　済"      TO         M-KEIJYO
          ELSE
              MOVE   SPACE               TO         M-KEIJYO
          END-IF
              WRITE      P-REC   FROM    MIDASI-1   AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
     END-IF.
     MOVE         6       TO      L-CNT.
 MIDASI-SEC-EXIT.
     EXIT.
************************************************************
*      __２    プリント処理                              *
************************************************************
 PRINT-SEC              SECTION.
     IF       IX   =   1
*---- 明細１編集
         MOVE     TBL-NYUK-F01(IX)      TO      M1-DENKB
                                                JYO-F02
*      ( 伝票区分名取得／条件ファイル検索 )
         MOVE     1                     TO      JYO-F01
         READ     HJYOKEN
           INVALID
             MOVE     ALL NC"＊"        TO      M1-DENKBNM
           NOT  INVALID
             MOVE     JYO-F03           TO      M1-DENKBNM
         END-READ
*
         MOVE     TBL-NYUK-F04(IX)      TO      M1-SOUSAI
         MOVE     TBL-NYUK-F23(IX)(3:2) TO      M1-NOHINYY
         MOVE     TBL-NYUK-F23(IX)(5:2) TO      M1-NOHINMM
         MOVE     TBL-NYUK-F23(IX)(7:2) TO      M1-NOHINDD
         MOVE     TBL-NYUK-F02(IX)      TO      M1-HACHUNO
         MOVE     TBL-NYUK-F03(IX)      TO      M1-EDABAN
         MOVE     TBL-NYUK-F29(IX)(3:2) TO      M1-HACHUYY
         MOVE     TBL-NYUK-F29(IX)(5:2) TO      M1-HACHUMM
         MOVE     TBL-NYUK-F29(IX)(7:2) TO      M1-HACHUDD
         MOVE     TBL-NYUK-F24(IX)      TO      M1-BASYO
         MOVE     WK-SOKNM              TO      M1-SOKONM
         MOVE     TBL-NYUK-F06(IX)      TO      M1-RYOHAN
         MOVE     TBL-NYUK-F09(IX)      TO      M1-ZEIKB
*      ( メモ取得／発注ファイル検索 )
         MOVE     TBL-NYUK-F01(IX)      TO      HAC-F01
         MOVE     TBL-NYUK-F02(IX)      TO      HAC-F02
         MOVE     ZERO                  TO      HAC-F03
         MOVE     ZERO                  TO      HAC-F04
         MOVE     TBL-NYUK-F05(IX)      TO      HAC-F05
         READ     ZHACHDT
           INVALID
             MOVE     ALL "*"           TO      M1-MEMO
             MOVE     ZERO              TO      WK-HAC-F22
           NOT INVALID KEY
             MOVE     HAC-F18           TO      M1-MEMO
             MOVE     HAC-F22           TO      WK-HAC-F22
         END-READ
*
*---- 明細２編集
         MOVE     TBL-NYUK-F07(IX)      TO      M2-SHIRECD
                                                SHI-F01
*      ( 仕入先名取得／仕入マスタ検索 )
         READ     ZSHIMS
           INVALID
             MOVE     ALL NC"＊"        TO      M2-SHIRENM
           NOT  INVALID
             MOVE     SHI-F02           TO      M2-SHIRENM
         END-READ
*
         IF       TBL-NYUK-F08(IX)      NOT =   ZERO
                  MOVE     TBL-NYUK-F08(IX) TO
                                        M2-TOKUICD
                                        TOK-F01
*      ( 取引先名取得／取引先マスタ検索 )
              READ     HTOKMS
                INVALID
                  MOVE     ALL NC"＊"        TO      M2-TOKUINM
                NOT  INVALID
                  MOVE     TOK-F03           TO      M2-TOKUINM
              END-READ
         END-IF
*
*94.12.20 START
         MOVE     SPACE                      TO      M2-NOUNYU
*94.12.20 END
         IF       TBL-NYUK-F12(IX)      NOT =   ZERO
              MOVE     TBL-NYUK-F08(IX)      TO      TEN-F52
              MOVE     TBL-NYUK-F12(IX)      TO      M2-NONYUCD
                                                TEN-F011
*      ( 納入先名取得／店舗マスタ検索 )
              READ     HTENMS
                INVALID
                  MOVE     ALL NC"＊"        TO      M2-NONYUNM
                NOT  INVALID
                  MOVE     TEN-F03           TO      M2-NONYUNM
              END-READ
         END-IF
*
         MOVE     SPACE         TO        P-REC
         WRITE    P-REC
         WRITE    P-REC         FROM      MEISAI-1    AFTER 1
         WRITE    P-REC         FROM      MEISAI-2    AFTER 1
         ADD      3             TO        L-CNT
     END-IF.
*---- 明細３編集
     MOVE     SPACE             TO        MEISAI-3.
     MOVE     TBL-NYUK-F05(IX)  TO        M3-GYONO.
     MOVE     TBL-NYUK-F01(IX)      TO      HAC-F01.
     MOVE     TBL-NYUK-F02(IX)      TO      HAC-F02.
     MOVE     ZERO                  TO      HAC-F03.
     MOVE     ZERO                  TO      HAC-F04.
     MOVE     TBL-NYUK-F05(IX)      TO      HAC-F05.
     READ     ZHACHDT
       NOT INVALID KEY
       MOVE     TBL-NYUK-F91(IX)  TO        M3-KANRYO
       IF       TBL-NYUK-F91(IX)  NOT =     "0" AND
                                            "1" AND
                                            "9"
             MOVE     HAC-F06     TO        M3-KANRYO
       END-IF
     END-READ.
*****MOVE     HAC-F06           TO        M3-KANRYO.
     MOVE     TBL-NYUK-F13(IX)  TO        M3-SHOCD    MEI-F011.
     MOVE     TBL-NYUK-F14(IX)  TO        M3-HINTAN   MEI-F012.
*    ( 商品名取得／商品名称マスタ検索 )
     READ     HMEIMS
       INVALID
         MOVE     ALL NC"＊"    TO        M3-SHONM
       NOT  INVALID
         MOVE     MEI-F021      TO        M3-SHONM(01:15)
         MOVE     MEI-F022      TO        M3-SHONM(16:15)
     END-READ.
*
*****MOVE     WK-HAC-F22        TO        M3-HACHUSU.
     MOVE     TBL-NYUK-F15(IX)  TO        M3-NYUKOSU.
     IF      (TBL-NYUK-F01(IX)  =     50  OR 60 )
         AND (TBL-NYUK-F04(IX)  =   ZERO        )
         MOVE     TBL-NYUK-F31(IX)  TO        M3-HACHUSU
*********MOVE     TBL-NYUK-F15(IX)  TO        M3-NYUKOSU
*********COMPUTE  WK-HACHZAN   =
*********         WK-HAC-F22   -          TBL-NYUK-F15(IX)
         COMPUTE  WK-HACHZAN   =
                  TBL-NYUK-F31(IX)  -     TBL-NYUK-F15(IX)
**
******** IF       WK-HACHZAN  <=   ZERO
********    THEN
********      MOVE   ZERO        TO        M3-HACHUZAN
******** ELSE
********      MOVE   WK-HACHZAN  TO        M3-HACHUZAN
******** END-IF
              MOVE   WK-HACHZAN  TO        M3-HACHUZAN
     END-IF.
     MOVE     TBL-NYUK-F21(IX)      TO        M3-BIKOU.
     IF  TBL-NYUK-F22(IX)   NOT =  SPACE
       MOVE   TBL-NYUK-F22(IX)(1:1) TO        M3-TANABAN1
       MOVE   "-"                   TO        M3-FILLER1
       MOVE   TBL-NYUK-F22(IX)(2:3) TO        M3-TANABAN2
       MOVE   "-"                   TO        M3-FILLER2
       MOVE   TBL-NYUK-F22(IX)(5:2) TO        M3-TANABAN3
     END-IF.
*
     WRITE    P-REC         FROM      MEISAI-3    AFTER 1.
     ADD      1             TO        L-CNT.
 PRINT-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   ZNYUKDT   ZHACHDT   HTENMS   HMEIMS
              ZSHIMS HTOKMS    ZSOKMS    HJYOKEN  PRINTF.
     DISPLAY  "ﾆｭｳｺ ﾌｧｲﾙ     (IN) = " READ-CNT   UPON  CONS.
     DISPLAY  "ﾆｭｳｺ ﾌｧｲﾙ   (SKIP) = " SKIP-CNT   UPON  CONS.
     DISPLAY  "ﾆｭｳｺ ﾌｧｲﾙ    (OUT) = " SELECT-CNT UPON  CONS.
     DISPLAY  "ﾁｪｯｸ ﾘｽﾄ (ﾍﾟｰｼﾞｽｳ) = " P-CNT      UPON  CONS.
 END-SEC-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
