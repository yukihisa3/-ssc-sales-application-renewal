****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　発注消込入力チェックリスト発行　　*
*    作成日／作成者　　　：　2020/02/25   ASS.II               *
*    処理内容　　　　　　：　画面より出力条件を入力して　　　　*
*    　　　　　　　　　　　　発注消込入力チェックリストを　　　*
*                            出力する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0210L.
 AUTHOR.                NAV.II.
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
     SELECT   NYKFILF   ASSIGN  TO   DA-01-VI-NYKFILL2
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   NYUK-F27
                                                  NYUK-F02
                                                  NYUK-F03
                                                  NYUK-F04
                                                  NYUK-F05
                        FILE    STATUS       IS   NYUK-STATUS.
****<< 発注 ファイル.ヘッダ  >>*****************************
     SELECT   HACHEDF   ASSIGN  TO   DA-01-VI-HACHEDL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   HED-F02
                        FILE    STATUS       IS   HED-STATUS.
****<< 発注 ファイル.明細　  >>*****************************
     SELECT   HACMEIF   ASSIGN  TO   DA-01-VI-HACMEIL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   BDY-F02
                                                  BDY-F03
                        FILE    STATUS       IS   BDY-STATUS.
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
****<< 担当者マスタ         >>******************************
     SELECT   HTANMS    ASSIGN  TO   DA-01-VI-TANMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TAN-F01
                                                  TAN-F02
                        FILE    STATUS       IS   TAN-STATUS.
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
 FD    NYKFILF
       LABEL     RECORD    IS    STANDARD.
       COPY      NYKFILF   OF    XFDLIB
                 JOINING   NYUK  PREFIX.
****<< 発注ファイル ヘッダ  >>*****************************
 FD    HACHEDF
       LABEL     RECORD    IS    STANDARD.
       COPY      HACHEDF   OF    XFDLIB
                 JOINING   HED   PREFIX.
****<< 発注ファイル 明細　  >>*****************************
 FD    HACMEIF
       LABEL     RECORD    IS    STANDARD.
       COPY      HACMEIF   OF    XFDLIB
                 JOINING   BDY   PREFIX.
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
****<<担当者マスタ         >>******************************
 FD    HTANMS
        LABEL     RECORD    IS    STANDARD.
        COPY      HTANMS    OF    XFDLIB
                  JOINING   TAN   PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FVD02101          OF   XMDLIB.
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
     02 HED-STATUS           PIC  X(02).
     02 BDY-STATUS           PIC  X(02).
     02 TEN-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 SHI-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
     02 TAN-STATUS           PIC  X(02).
***  日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
***  画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
***  画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
****  フラグ                  ****
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE  SPACE.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(06).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  カウンタ                ****
 01  CNT-AREA.
     02  L-CNT               PIC  9(02)  VALUE  90.
     02  P-CNT               PIC  9(07)  VALUE  ZERO.
     02  READ-CNT            PIC  9(07)  VALUE  ZERO.
     02  SELECT-CNT          PIC  9(07)  VALUE  ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
     02  TBL-CNT             PIC  9(01)  VALUE  ZERO.
     02  PRINT-CNT           PIC  9(07)  VALUE  ZERO.
****  ワーク                  ****
 01  WORK-AREA.
     02  WK-SDATE            PIC  9(08).
     02  WK-SDATE-R   REDEFINES  WK-SDATE.
       03  FILLER            PIC  9(02).
       03  WK-SYMD.
         04  WK-SYY          PIC  9(02).
         04  WK-SMM          PIC  9(02).
         04  WK-SDD          PIC  9(02).
     02  WK-EDATE            PIC  9(08).
     02  WK-EDATE-R   REDEFINES  WK-EDATE.
       03  FILLER            PIC  9(02).
       03  WK-EYMD.
         04  WK-EYY          PIC  9(02).
         04  WK-EMM          PIC  9(02).
         04  WK-EDD          PIC  9(02).
     02  WK-HACHZAN          PIC S9(08)V99      VALUE  ZERO.
     02  WK-L-CNT            PIC  9(02)         VALUE  ZERO.
     02  WK-NYUK-KEY                     VALUE  ZERO.
       03  WK-NYUK-F27       PIC  X(02).
       03  WK-NYUK-F01       PIC  9(02).
       03  WK-NYUK-F02       PIC  9(07).
       03  WK-NYUK-F03       PIC  9(02).
       03  WK-NYUK-F04       PIC  9(01).
     02  WK-HIKAKU-KEY.
       03  WK-HIKA-F27       PIC  X(02).
       03  FILLER            PIC  X(12).
****  インデックス            ****
 01  IX                      PIC  9(01).
****  入庫ファイルテーブル    ****
      COPY      NYKFILF   OF    XFDLIB
                JOINING   TBL   PREFIX.
 01  TBL-NYUKF.
     02  TBL-NYUK        OCCURS  6.
       03  FILLER            PIC  X(200).
****  見出し行０             ****
 01  MIDASI-0.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "NVD0210L".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-KEIJYO            PIC  N(06)
                             CHARACTER   TYPE   IS   YB2.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(19)  VALUE
         NC"＊＊＊　発注消込チェックリスト　＊＊＊"
                             CHARACTER   TYPE   IS   YB2.
     02  M-TOKHAN            PIC  N(08)
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
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
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"倉庫："
                             CHARACTER   TYPE   IS   NIHONGO.
     02  H1-SOKCD            PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-SOKNM            PIC  N(18)
                             CHARACTER   TYPE   IS   NIHONGO.
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
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"登録日".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"修正日".
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"仕入先".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  X(02)  VALUE  "DT".
     02  FILLER              PIC  N(02)  VALUE  NC"種別".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  X(04)  VALUE  "D365".
     02  FILLER              PIC  N(04)  VALUE  NC"発注番号".
     02  FILLER              PIC  X(29)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"担当者".
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
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"_　番".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
****  見出し行５             ****
 01  MIDASI-5       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(67)  VALUE  ALL NC"─".
****  明細行０             ****
 01  MEISAI-0.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(134) VALUE  ALL "-".
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
     02  M1-NOHINYY          PIC  99.
     02  M1-P1               PIC  X(01)  VALUE  ".".
     02  M1-NOHINMM          PIC  99.
     02  M1-P2               PIC  X(01)  VALUE  ".".
     02  M1-NOHINDD          PIC  99.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M1-HACHUNO          PIC  9(07).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M1-EDABAN           PIC  9(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M1-HACHUYY          PIC  99.
     02  M1-P3               PIC  X(01)  VALUE  ".".
     02  M1-HACHUMM          PIC  99.
     02  M1-P4               PIC  X(01)  VALUE  ".".
     02  M1-HACHUDD          PIC  99.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M1-BASYO            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M1-SOKONM           PIC  N(05)
                             CHARACTER     TYPE   IS   YB1.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M1-RYOHAN           PIC  9(09).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  M1-ZEIKB            PIC  X(01).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  M1-MEMO             PIC  X(07).
     02  FILLER              PIC  X(23)  VALUE  SPACE.
     02  M1-TDT1             PIC  99.
     02  M1-P5               PIC  X(01)  VALUE  ".".
     02  M1-TDT2             PIC  99.
     02  M1-P6               PIC  X(01)  VALUE  ".".
     02  M1-TDT3             PIC  99.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  M1-SDT1             PIC  99.
     02  M1-P7               PIC  X(01)  VALUE  ".".
     02  M1-SDT2             PIC  99.
     02  M1-P8               PIC  X(01)  VALUE  ".".
     02  M1-SDT3             PIC  99.
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB1.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  M2-SHIRECD          PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M2-SHIRENM          PIC  N(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M2-DTSYUCD          PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M2-DTSYUNM          PIC  N(05).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M2-HACYUK1          PIC  X(01).
     02  M2-HACYUNO          PIC  X(20).
     02  M2-HACYUK2          PIC  X(01).
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  M2-TANCD            PIC  X(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M2-TANNM            PIC  N(10).
*2013/12/18↓
***  02  FILLER              PIC  X(32)  VALUE  SPACE.
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  M2-TITOL            PIC  N(05)  VALUE  NC"計上税区：".
     02  M2-ZEINM            PIC  N(05).
*****02  FILLER              PIC  X(59)  VALUE  SPACE.
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
     02  M3-HACHUSU          PIC  Z,ZZZ,ZZ9.99.
     02  M3-NYUKOSU          PIC  Z,ZZZ,ZZ9.99.
     02  M3-HACHUZAN         PIC  -,---,--9.99.
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  M3-TANABAN          PIC  X(06).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01              PIC N(20) VALUE
                             NC"_取消　_終了".
     02  PMSG02              PIC N(20) VALUE
                             NC"_取消　_終了　_項目戻り".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0210L".
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
            NC"誤ったＰＦキーが押されました。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"倉庫コードを入力して下さい。".
     02  MSG-ERR3            PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     02  MSG-ERR4            PIC  N(20)  VALUE
            NC"指定の倉庫コードは扱えません。".
     02  MSG-ERR5            PIC  N(20)  VALUE
            NC"年月日に誤りがあります。".
     02  MSG-ERR6            PIC  N(20)  VALUE
            NC"計上区分に誤りがあります。".
     02  MSG-ERR7            PIC  N(20)  VALUE
            NC"計上区分は入力できません。".
     02  MSG-ERR8            PIC  N(20)  VALUE
            NC"開始が終了を越えています。".
 01  ERR-MSG-ALL             REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS   8  TIMES.
****  エラーメッセージ　ＦＯＲ　コンソール  ***
 01  FILE-ERROR.
     02  FILE-ERR1           PIC  N(10)  VALUE
            NC"入庫ファイル　異常！".
     02  FILE-ERR2           PIC  N(10)  VALUE
            NC"発注（ヘッダ）異常！".
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
     02  FILE-ERR10          PIC  N(10)  VALUE
            NC"担当者マスタ　異常！".
***  日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**** パラメータ               ****
 LINKAGE                SECTION.
 01  LINK-SOKCD         PIC  X(02).
 01  LINK-DSOKCD        PIC  X(02).
 01  LINK-BUMON         PIC  X(04).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION      USING   LINK-SOKCD
                                              LINK-DSOKCD
                                              LINK-BUMON.
*
 DECLARATIVES.
**入庫ファイル
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKFILF.
     MOVE   "NYKFILF "        TO    ERR-FL-ID.
     MOVE    NYUK-STATUS      TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**発注ファイル（ヘッダ）
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACHEDF.
     MOVE   "HACHEDF "        TO    ERR-FL-ID.
     MOVE    HED-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**発注ファイル（明細）
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACMEIF.
     MOVE   "HACHEDF "        TO    ERR-FL-ID.
     MOVE    BDY-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**商品名称マスタ
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**仕入先マスタ
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSHIMS.
     MOVE   "ZSHIMS  "        TO    ERR-FL-ID.
     MOVE    SHI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**取引先マスタ
 FILEERR-SEC7           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTOKMS.
     MOVE   "HTOKMS  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**倉庫マスタ
 FILEERR-SEC8           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**条件ファイル
 FILEERR-SEC9           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**画面ファイル
 FILEERR-SEC10          SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**担当者マスタ
 FILEERR-SEC11          SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTANMS.
     MOVE   "HTANMS  "        TO    ERR-FL-ID.
     MOVE    TAN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 FNY00201B-START         SECTION.
     DISPLAY  "**  NVD0210L   START  **"   UPON  CONS.
*************************
     MOVE  "01"          TO   LINK-SOKCD
     MOVE  "01"          TO   LINK-DSOKCD
     MOVE  "01"          TO   LINK-BUMON.
*
*    DISPLAY "倉庫CD    =" LINK-SOKCD
*    DISPLAY "代表倉庫CD=" LINK-DSOKCD
*    DISPLAY "部門      =" LINK-BUMON
*************************
*
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC      UNTIL  END-FLG  =  "END".
     PERFORM       END-SEC.
*
     DISPLAY  "**  NVD0210L    END   **"   UPON  CONS.
     STOP     RUN.
 FNY00201B-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     NYKFILF.
     OPEN     INPUT     HACHEDF.
     OPEN     INPUT     HACMEIF.
***     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     ZSHIMS.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HJYOKEN.
     OPEN     INPUT     HTANMS.
     OPEN     OUTPUT    PRINTF.
*---- システム日付・時刻の取得
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
*---- 画面・帳票日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY  H-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM    H-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD    H-DD.
*---- システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*---- 画面時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
*---- システム日付　取得
     ACCEPT   SYSYMD    FROM   DATE.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"    TO    HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03             TO    HEN-TOKHAN
     END-READ.
     MOVE     HEN-TOKHAN-AREA     TO   M-TOKHAN.
*
     PERFORM     WRITE-SELECT-SEC.
*入庫ファイル読み込み
     IF    END-FLG  NOT =   "END"
        PERFORM     NYKFILF-START-SEC
     END-IF.
     IF    END-FLG  NOT =   "END"
        PERFORM     NYKFILF-READ-SEC
     END-IF.
 INIT-SEC-EXIT.
     EXIT.
****************************************************************
*      ■１　　出力条件指定　処理                              *
****************************************************************
 WRITE-SELECT-SEC            SECTION.
*----- 初画面出力 ------*
     PERFORM    DSP-INIT-SEC.
*
 WS010.
*----- 倉庫ＣＤプロテクト ----*
     IF    LINK-DSOKCD  NOT =  "01"  AND  "88"
       MOVE  "X"                 TO  EDIT-STATUS  OF  SOKCD
       MOVE  "X"                 TO  EDIT-STATUS  OF  KEIJYO
     END-IF.
*----- 画面ＲＥＡＤ ----*
     MOVE    "BODY"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　_１ ----*
     PERFORM    OPT-CLR-SEC.
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
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS010
     END-EVALUATE.
 WS111.
*----- 入力項目チェック---*
*（倉庫チェック）*
     IF  LINK-DSOKCD  =   "01"  OR  "88"
         IF   SOKCD   =   SPACE
             MOVE    NC"全倉庫"      TO   SOKNM
         ELSE
             MOVE    SOKCD           TO   SOK-F01
             READ    ZSOKMS
                 INVALID KEY
                   MOVE  SPACE   TO   SOKNM
                   MOVE  "R"     TO   EDIT-OPTION   OF   SOKCD
                   MOVE  "C"     TO   EDIT-CURSOR   OF   SOKCD
                   IF   ERR-CD    =    ZERO
                      MOVE    3      TO   ERR-CD
                   END-IF
               NOT INVALID KEY
                   MOVE    SOK-F02   TO   SOKNM
             END-READ
         END-IF
     END-IF.
 WS112.
*
*（開始年月日チェック）*
     MOVE     SYY                 TO   WK-SYY.
     MOVE     SMM                 TO   WK-SMM.
     MOVE     SDD                 TO   WK-SDD.
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     WK-SYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET    =  ZERO
           MOVE   "3"             TO   LINK-IN-KBN
           MOVE   WK-SYMD         TO   LINK-IN-YMD6
           CALL   "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
           MOVE   LINK-OUT-YMD    TO   WK-SDATE
     ELSE
           MOVE   ZERO            TO   WK-SDATE
           MOVE    "R"       TO   EDIT-OPTION   OF   SYY
                                  EDIT-OPTION   OF   SMM
                                  EDIT-OPTION   OF   SDD
           MOVE    "C"       TO   EDIT-CURSOR   OF   SYY
           IF  ERR-CD    =    ZERO
               MOVE   5      TO    ERR-CD
           END-IF
     END-IF.
*（終了年月日チェック）*
     MOVE     EYY                 TO   WK-EYY.
     MOVE     EMM                 TO   WK-EMM.
     MOVE     EDD                 TO   WK-EDD.
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     WK-EYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET    =  ZERO
           MOVE   "3"             TO   LINK-IN-KBN
           MOVE   WK-EYMD         TO   LINK-IN-YMD6
           CALL   "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
           MOVE   LINK-OUT-YMD    TO   WK-EDATE
     ELSE
           MOVE   ZERO            TO   WK-EDATE
           MOVE    "R"       TO   EDIT-OPTION   OF   EYY
                                  EDIT-OPTION   OF   EMM
                                  EDIT-OPTION   OF   EDD
           MOVE    "C"       TO   EDIT-CURSOR   OF   EYY
           IF  ERR-CD    =    ZERO
               MOVE   5      TO    ERR-CD
           END-IF
     END-IF.
*（開始・終了の関連チェック）
     IF       WK-SDATE  NOT =  ZERO
         AND  WK-EDATE  NOT =  ZERO
         AND  WK-SDATE    >    WK-EDATE
           MOVE    "R"       TO   EDIT-OPTION   OF   SYY
                                  EDIT-OPTION   OF   SMM
                                  EDIT-OPTION   OF   SDD
                                  EDIT-OPTION   OF   EYY
                                  EDIT-OPTION   OF   EMM
                                  EDIT-OPTION   OF   EDD
           MOVE    "C"       TO   EDIT-CURSOR   OF   SYY
           IF  ERR-CD    =    ZERO
               MOVE   8      TO    ERR-CD
           END-IF
     END-IF.
*
*（計上のチェック）*
     IF  KEIJYO      NOT    NUMERIC
         IF    LINK-DSOKCD   =   "01"  OR  "88"
               MOVE     ZERO      TO   KEIJYO
         ELSE
               MOVE     9         TO   KEIJYO
         END-IF
     END-IF.
     IF  LINK-DSOKCD    =   "01"  OR  "88"
         IF  KEIJYO      NOT =       ZERO AND 1 AND 9
             MOVE    "R"         TO   EDIT-OPTION   OF   KEIJYO
             MOVE    "C"         TO   EDIT-CURSOR   OF   KEIJYO
             IF       ERR-CD    =    ZERO
                      MOVE    6      TO   ERR-CD
             END-IF
         END-IF
     END-IF.
     IF (LINK-DSOKCD  NOT =       "01"  AND  "88" )  AND
         KEIJYO       NOT =       9
         MOVE    "R"         TO   EDIT-OPTION   OF   KEIJYO
         MOVE    "C"         TO   EDIT-CURSOR   OF   KEIJYO
         IF       ERR-CD    =    ZERO
                  MOVE    7      TO   ERR-CD
         END-IF
     END-IF.
*
*----- エラーが有った時 ----*
     IF   ERR-CD  NOT =   ZERO
       MOVE    "BODY"        TO   DSP-GROUP
       PERFORM    DSP-ERR-SEC
       GO                    TO   WS010
     END-IF.
*
*----- ボディー部の出力 ----*
     PERFORM    OPT-CLR-SEC.
     MOVE    PMSG02          TO   PFKGID.
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
         WHEN     "F006"
             GO                  TO   WS010
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
*      ■■１　初期画面　出力　処理                            *
****************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE    SPACE               TO   FVD02101.
     MOVE    SPACE               TO   DSP-CONTROL.
     MOVE    "CL"                TO   DSP-PROC.
     MOVE    "FVD02101"          TO   DSP-FORMAT.
     MOVE    "SCREEN"            TO   DSP-GROUP.
*
     MOVE    SYS-YY              TO   SYY  EYY.
     MOVE    SYS-MM              TO   SMM  EMM.
     MOVE    SYS-DD              TO   SDD  EDD.
     MOVE    PMSG01              TO   PFKGID.
*
     IF      LINK-DSOKCD    NOT =    "01"  AND  "88"
         MOVE    LINK-SOKCD      TO   SOK-F01   SOKCD
         READ    ZSOKMS
             INVALID KEY
               CONTINUE
             NOT INVALID KEY
               MOVE    SOK-F02   TO   SOKNM
         END-READ
         MOVE    9               TO   KEIJYO
     END-IF.
     MOVE    HEN-DATE            TO   SDATE.
     MOVE    HEN-TIME            TO   STIME.
     MOVE    HEN-TOKHAN-AREA     TO   TOKHAN.
*
     WRITE                  FVD02101.
*
 DSP-INIT-END.
     EXIT.
****************************************************************
*      ■■２　画面ＲＥＡＤ　処理                              *
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
*      ■■３　画面ＷＲＩＴＥ　処理                            *
****************************************************************
 DSP-WRITE-SEC          SECTION.
     MOVE    SPACE               TO   DSP-PROC.
     WRITE                  FVD02101.
 DSP-WRITE-END.
     EXIT.
****************************************************************
*      1.1.4     エラーメッセージセット　処理                  *
****************************************************************
 DSP-ERR-SEC                 SECTION.
*---- エラー メッセージ セット ----*
     MOVE    ERR-MSG(ERR-CD)     TO   ERRMSG.
     MOVE    ZERO                TO   ERR-CD.
     PERFORM    DSP-WRITE-SEC.
 DSP-ERR-END.
     EXIT.
****************************************************************
*      1.2       入庫ファイル　ＳＴＡＲＴ処理                *
****************************************************************
 NYKFILF-START-SEC      SECTION.
     INITIALIZE                  NYUK-REC.
     MOVE  SOKCD             TO  NYUK-F27.
*
     START NYKFILF   KEY >=      NYUK-F27  NYUK-F02  NYUK-F03
                                 NYUK-F04  NYUK-F05
         INVALID
            MOVE   "END"     TO  END-FLG
     END-START.
 NYKFILF-START-EXIT.
     EXIT.
************************************************************
*      1.3       入庫ファイル　ＲＥＡＤ処理                *
************************************************************
 NYKFILF-READ-SEC       SECTION.
     READ    NYKFILF
       AT  END
           MOVE     "END"        TO   END-FLG
           MOVE     HIGH-VALUE   TO   WK-NYUK-KEY
           GO       TO   NYKFILF-READ-EXIT
     END-READ.
     ADD      1          TO   READ-CNT.
*
 NYKFILF-010.
     IF   SOKCD    NOT =   SPACE     AND
          SOKCD    NOT =   NYUK-F27
           MOVE     "END"        TO   END-FLG
           MOVE     HIGH-VALUE   TO   WK-NYUK-KEY
           GO       TO   NYKFILF-READ-EXIT
     END-IF.
 NYKFILF-020.
*伝票区分
     IF   LINK-DSOKCD     =   "01"  OR  "88"
          IF    NYUK-F01   NOT =   "50"
               GO          TO   NYKFILF-READ-SEC
          END-IF
     END-IF.
 NYKFILF-030.
*計上
     EVALUATE   KEIJYO
       WHEN   1
          IF    NYUK-F30   =       ZERO
               GO          TO   NYKFILF-READ-SEC
          END-IF
       WHEN   9
          IF    NYUK-F30   NOT =   ZERO
               GO          TO   NYKFILF-READ-SEC
          END-IF
     END-EVALUATE.
*
 NYKFILF-040.
     IF  KEIJYO  =  9
         IF ( WK-SDATE    <=    NYUK-F98 ) AND
            ( WK-EDATE    >=    NYUK-F98 ) AND
            ( NYUK-F01     =    50 )
                ADD      1             TO   SELECT-CNT
                MOVE     NYUK-F27      TO   WK-NYUK-F27
                MOVE     NYUK-F01      TO   WK-NYUK-F01
                MOVE     NYUK-F02      TO   WK-NYUK-F02
                MOVE     NYUK-F03      TO   WK-NYUK-F03
                MOVE     NYUK-F04      TO   WK-NYUK-F04
                GO       TO                 NYKFILF-READ-EXIT
            ELSE
                ADD  1          TO   SKIP-CNT
                GO              TO   NYKFILF-READ-SEC
         END-IF
     END-IF.
*
 NYKFILF-050.
**** DISPLAY "WK-SDATE = " WK-SDATE UPON CONS.
**** DISPLAY "WK-EDATE = " WK-EDATE UPON CONS.
**** DISPLAY "NYUK-F98 = " NYUK-F98 UPON CONS.
**** DISPLAY "NYUK-F99 = " NYUK-F99 UPON CONS.
     IF ((WK-SDATE <= NYUK-F98  AND  WK-EDATE >= NYUK-F98) OR
         (WK-SDATE <= NYUK-F99  AND  WK-EDATE >= NYUK-F99))
       THEN
           ADD      1             TO   SELECT-CNT
           MOVE     NYUK-F27      TO   WK-NYUK-F27
           MOVE     NYUK-F01      TO   WK-NYUK-F01
           MOVE     NYUK-F02      TO   WK-NYUK-F02
           MOVE     NYUK-F03      TO   WK-NYUK-F03
           MOVE     NYUK-F04      TO   WK-NYUK-F04
       ELSE
         ADD  1          TO   SKIP-CNT
         GO              TO   NYKFILF-READ-SEC
     END-IF.
 NYKFILF-READ-EXIT.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
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
*      ■1       テーブル格納処理                          *
************************************************************
 TBL-SET-SEC            SECTION.
     MOVE     NYUK-REC      TO       TBL-NYUK(IX).
     ADD      1             TO       TBL-CNT.
     PERFORM  NYKFILF-READ-SEC.
  TBL-SET-EXIT.
************************************************************
*      ■２      編集処理                                  *
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
*****IF       WK-L-CNT     >       62
     IF       WK-L-CNT     >       52
        OR    H1-SOKCD     NOT =   WK-HIKA-F27
         PERFORM       MIDASI-SEC
     END-IF.
*
     PERFORM   PRINT-SEC    VARYING   IX  FROM  1  BY  1
               UNTIL        IX   >    TBL-CNT.
*
     MOVE     SPACE         TO        P-REC.
*****WRITE    P-REC         AFTER     1.
     WRITE    P-REC         FROM      MEISAI-0    AFTER 1
     ADD      1             TO        L-CNT.
*
     MOVE     ZERO          TO        TBL-CNT.
 HNSYU-SEC-EXIT.
     EXIT.
************************************************************
*      2.■1       見出し処理                              *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE         SPACE   TO      P-REC.
     INITIALIZE                   P-REC.
     ADD          1       TO      P-CNT.
     MOVE         P-CNT   TO      H-PAGE.
     MOVE   WK-HIKA-F27   TO      H1-SOKCD
                                  SOK-F01.
     READ    ZSOKMS
         INVALID KEY
               MOVE    SPACE     TO   H1-SOKNM
         NOT INVALID
               MOVE    SOK-F02   TO   H1-SOKNM
     END-READ.
     IF       KEIJYO =   1
              MOVE   NC"計　上　済"      TO      M-KEIJYO
     ELSE
         IF   KEIJYO =   9
              MOVE   NC"未　計　上"      TO      M-KEIJYO
         ELSE
              MOVE   SPACE               TO      M-KEIJYO
         END-IF
     END-IF.
     IF       P-CNT  >   1
              WRITE      P-REC   AFTER   PAGE
     END-IF.
     WRITE      P-REC   FROM    MIDASI-0   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-5   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-3   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-4   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-5   AFTER  1.
     MOVE         8       TO      L-CNT.
 MIDASI-SEC-EXIT.
     EXIT.
************************************************************
*      ■■２    プリント処理                              *
************************************************************
 PRINT-SEC              SECTION.
     MOVE     TBL-NYUK(IX)  TO       TBL-REC.
*
     IF       IX   =   1
         MOVE     SPACE                 TO      MEISAI-1 MEISAI-2
*---- 明細１編集
         MOVE     TBL-F01               TO      M1-DENKB
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
         MOVE     TBL-F04               TO      M1-SOUSAI
         MOVE     TBL-F26(3:2)          TO      M1-NOHINYY
         MOVE     TBL-F26(5:2)          TO      M1-NOHINMM
         MOVE     TBL-F26(7:2)          TO      M1-NOHINDD
         MOVE     "."                   TO      M1-P1  M1-P2
         MOVE     TBL-F02               TO      M1-HACHUNO
         MOVE     TBL-F03               TO      M1-EDABAN
         MOVE     TBL-F13(3:2)          TO      M1-HACHUYY
         MOVE     TBL-F13(5:2)          TO      M1-HACHUMM
         MOVE     TBL-F13(7:2)          TO      M1-HACHUDD
         MOVE     "."                   TO      M1-P3  M1-P4
         MOVE     TBL-F27               TO      M1-BASYO
         MOVE     H1-SOKNM              TO      M1-SOKONM
         IF       TBL-F07   NOT =  ZERO
                  MOVE   TBL-F07        TO      M1-RYOHAN
         END-IF
         MOVE     TBL-F10               TO      M1-ZEIKB
*      ( メモ取得／発注ファイル検索 )
         MOVE     TBL-F02               TO      HED-F02
         READ     HACHEDF
           INVALID
             MOVE     ALL "*"           TO      M1-MEMO
           NOT INVALID KEY
             MOVE     HED-F15           TO      M1-MEMO
         END-READ
*
*---- 明細２編集
         MOVE     TBL-F08               TO      M2-SHIRECD
                                                SHI-F01
*      ( 仕入先名取得／仕入マスタ検索 )
         READ     ZSHIMS
           INVALID
             MOVE     ALL NC"＊"        TO      M2-SHIRENM
           NOT  INVALID
             MOVE     SHI-F02           TO      M2-SHIRENM
         END-READ
*
         IF       TBL-F09               NOT =   ZERO
              MOVE    TBL-F09           TO      TOK-F01
         END-IF
*
         MOVE     TBL-F91               TO      M2-DTSYUCD
         MOVE     87                    TO      JYO-F01
         MOVE     TBL-F91               TO      JYO-F02
*      ( データ種別取得／条件ファイル検索 )
         READ     HJYOKEN
                INVALID
                  MOVE     ALL NC"＊"   TO      M2-DTSYUNM
                NOT  INVALID
                  MOVE     JYO-F03      TO      M2-DTSYUNM
         END-READ
         MOVE     HED-F313              TO      M2-HACYUNO
         MOVE     "("                   TO      M2-HACYUK1
         MOVE     ")"                   TO      M2-HACYUK2
*
*      ( 取引先名取得／取引先マスタ検索 )
****              READ     HTOKMS
****                INVALID
****                  MOVE     ALL NC"＊"        TO      M2-TOKU
****                NOT  INVALID
****                  MOVE     TOK-F03           TO      M2-TOKUIN
****              END-READ
****         END-IF
*
****         MOVE     SPACE                      TO      M2-NOUNYU
***         IF       TBL-F12               NOT =   ZERO
***              MOVE     TBL-F09               TO      TEN-F52
***              MOVE     TBL-F12               TO      M2-NONYUCD
***                                                     TEN-F011
*      ( 納入先名取得／店舗マスタ検索 )
***              READ     HTENMS
***                INVALID
***                  MOVE     ALL NC"＊"        TO      M2-NONYU
***                NOT  INVALID
***                  MOVE     TEN-F03           TO      M2-NONYUNM
***              END-READ
***         END-IF
*
         MOVE     SPACE                      TO      M2-TANCD
         IF       TBL-F33               NOT =   SPACE
              MOVE     LINK-BUMON            TO      TAN-F01
              MOVE     TBL-F33               TO      M2-TANCD
                                                     TAN-F02
*      ( 担当者名取得／担当者マスタ検索 )
              READ     HTANMS
                INVALID
                  MOVE     ALL NC"＊"        TO      M2-TANNM
                NOT  INVALID
                  MOVE     TAN-F03           TO      M2-TANNM
              END-READ
         END-IF
*
*2013/12/18↓　計上税区分
         MOVE     NC"計上税区："             TO      M2-TITOL
*2014/01/06↓　
*        IF       TBL-F10               NOT =   ZERO
         IF       TBL-F10               NOT =   SPACE
*2014/01/06↑　
              MOVE     50                    TO      JYO-F01
              MOVE     TBL-F10               TO      JYO-F02
*      ( 税区分名取得／条件ファイル検索 )
              READ     HJYOKEN
                INVALID
                  MOVE     ALL NC"＊"        TO      M2-ZEINM
                NOT  INVALID
                  MOVE     JYO-F03           TO      M2-ZEINM
              END-READ
         ELSE
              MOVE         SPACE             TO      M2-ZEINM
         END-IF
*2013/12/18↑　計上税区分
*
*        登録日セット
         MOVE     "."           TO        M1-P5  M1-P6
         MOVE     TBL-F98(3:2)  TO        M1-TDT1
         MOVE     TBL-F98(5:2)  TO        M1-TDT2
         MOVE     TBL-F98(7:2)  TO        M1-TDT3
*        修正日セット
         MOVE     "."           TO        M1-P7  M1-P8
         MOVE     TBL-F99(3:2)  TO        M1-SDT1
         MOVE     TBL-F99(5:2)  TO        M1-SDT2
         MOVE     TBL-F99(7:2)  TO        M1-SDT3
*
         WRITE    P-REC         FROM      MEISAI-1    AFTER 1
         WRITE    P-REC         FROM      MEISAI-2    AFTER 1
         ADD      2             TO        L-CNT
     END-IF.
*---- 明細３編集
     MOVE     SPACE             TO        MEISAI-3.
     MOVE     TBL-F05           TO        M3-GYONO.
     MOVE     TBL-F02           TO        BDY-F02.
     MOVE     TBL-F05           TO        BDY-F03.
     READ     HACMEIF
       INVALID
                CONTINUE
       NOT INVALID KEY
       MOVE     TBL-F06           TO        M3-KANRYO
       IF       TBL-F06           NOT =     "0" AND
                                            "1" AND
                                            "9"
             MOVE     BDY-F05     TO        M3-KANRYO
       END-IF
     END-READ.
     MOVE     TBL-F15           TO        M3-SHOCD    MEI-F011.
     MOVE     TBL-F16           TO        M3-HINTAN   MEI-F012.
*    ( 商品名取得／商品名称マスタ検索 )
     READ     HMEIMS
       INVALID
         MOVE     ALL NC"＊"    TO        M3-SHONM
       NOT  INVALID
         MOVE     MEI-F021      TO        M3-SHONM(01:15)
         MOVE     MEI-F022      TO        M3-SHONM(16:15)
     END-READ.
*
     MOVE     TBL-F18           TO        M3-NYUKOSU.
     IF      (TBL-F01      =     50  OR 60 )
         AND (TBL-F04      =   ZERO        )
         MOVE     TBL-F25           TO        M3-HACHUSU
         COMPUTE  WK-HACHZAN   =    TBL-F25   -   TBL-F18
         MOVE     WK-HACHZAN        TO        M3-HACHUZAN
     END-IF.
     MOVE     TBL-F17               TO        M3-TANABAN.
*    IF  TBL-F17       NOT =  SPACE
*      MOVE   TBL-F17(1:1)          TO        M3-TANABAN1
*      MOVE   "-"                   TO        M3-FILLER1
*      MOVE   TBL-F17(2:3)          TO        M3-TANABAN2
*      MOVE   "-"                   TO        M3-FILLER2
*      MOVE   TBL-F17(5:2)          TO        M3-TANABAN3
*    END-IF.
*
     WRITE    P-REC         FROM      MEISAI-3    AFTER 1.
     ADD      1             TO        L-CNT.
 PRINT-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF     HTANMS
***              NYKFILF  HACHEDF  HACMEIF  HTENMS   HMEIMS
              NYKFILF  HACHEDF  HACMEIF  HMEIMS
              ZSHIMS   HTOKMS   ZSOKMS   HJYOKEN  PRINTF.
*
     DISPLAY "* NYKFILF (IN)=" READ-CNT   " *" UPON CONS.
     DISPLAY "* NYKFILF (OT)=" SELECT-CNT " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" P-CNT      " *" UPON CONS.
 END-SEC-EXIT.
     EXIT.
************************************************************
*                 項目属性初期化                           *
************************************************************
 OPT-CLR-SEC            SECTION.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SOKCD.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SYY.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SYY.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SMM.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SMM.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SDD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SDD.
     MOVE    "M"                 TO   EDIT-OPTION   OF   EYY.
     MOVE    " "                 TO   EDIT-CURSOR   OF   EYY.
     MOVE    "M"                 TO   EDIT-OPTION   OF   EMM.
     MOVE    " "                 TO   EDIT-CURSOR   OF   EMM.
     MOVE    "M"                 TO   EDIT-OPTION   OF   EDD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   EDD.
     MOVE    "M"                 TO   EDIT-OPTION   OF   KEIJYO.
     MOVE    " "                 TO   EDIT-CURSOR   OF   KEIJYO.
 OPT-CLR-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
