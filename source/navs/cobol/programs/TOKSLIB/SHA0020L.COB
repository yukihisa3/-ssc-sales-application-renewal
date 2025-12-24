****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　発注管理システム　　　　　　　　　*
*    モジュール名　　　　：　発注入力チェックリスト            *
*    作成日／更新日　　　：　00/04/26                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　発注入力チェックリストの出力を行う*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0020L.
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
***-<<  画面ファイル  >>************************************
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
*
***-<<  発注ファイル（ヘッダ）  >>*************************
     SELECT   HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   HED-F17
                                                 HED-F02
                        FILE      STATUS    IS   HED-STATUS.
*
***-<<  発注ファイル（明細）    >>*************************
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   BDY-F02
                                                 BDY-F03
                        FILE      STATUS    IS   BDY-STATUS.
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
****<<  画面ファイル  >>************************************
 FD  DSPF.
 COPY   FHA00201         OF       XMDLIB
        JOINING         DSP       PREFIX.
****<< 発注ファイル（ヘッダ） >>****************************
 FD  HACHEDF.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HED       PREFIX.
****<< 発注ファイル（明細）   >>****************************
 FD  HACMEIF.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   BDY       PREFIX.
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
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 HED-STATUS           PIC  X(2).
     02 BDY-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
     02 TOK-STATUS           PIC  X(2).
     02 TEN-STATUS           PIC  X(2).
     02 ZSH-STATUS           PIC  X(2).
**** 日付／時刻
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
**** 画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
**** 画面表示時刻編集
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
*
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
 01  OLD-KEY.
     03  OLD-F17             PIC  X(02).
     03  OLD-F02             PIC  9(07).
 01  NEW-KEY.
     03  NEW-F17             PIC  X(02).
     03  NEW-F02             PIC  9(07).
*
****  見出し行０             ****
 01  MIDASI0        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SHA0020L".
     02  FILLER              PIC  X(25)  VALUE  SPACE.
     02  FILLER              PIC  N(19)  VALUE
         NC"＊＊＊　発注入力チェックリスト　＊＊＊".
     02  H-TOKHAN            PIC  N(08)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZZ9.
****  見出し行１             ****
 01  MIDASI1        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"倉庫：".
     02  H1-SOKCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-SOKNM            PIC  N(18).
****  見出し行２             ****
 01  MIDASI2        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"伝票区分".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注_".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注日".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"納　期".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"受担".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"場　所".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"量販_".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"送　料".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"請求".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"税区".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"メモ".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"登録日".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"更新日".
****  見出し行３             ****
 01  MIDASI3        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"完".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"仕入先".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"得意先".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"納入先".
     02  FILLER              PIC  X(17)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"社外".
     02  FILLER              PIC  X(04)  VALUE  " MSG".
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"送り先".
     02  FILLER              PIC  X(89)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"摘　要".
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
****  見出し行６             ****
 01  MIDASI6        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(67)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI1        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT01               PIC  N(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT02               PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT03               PIC  N(05).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT04               PIC  9(07).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT05-YY            PIC  99.
     02  P1                  PIC  X(01).
     02  PRT05-MM            PIC  99.
     02  P2                  PIC  X(01).
     02  PRT05-DD            PIC  99.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT06-YY            PIC  99.
     02  P3                  PIC  X(01).
     02  PRT06-MM            PIC  99.
     02  P4                  PIC  X(01).
     02  PRT06-DD            PIC  99.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT07               PIC  X(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT08               PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT09               PIC  N(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT10               PIC  9(09).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  PRT11               PIC  9(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT12               PIC  ZZZ,ZZ9.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  PRT121              PIC  9(01).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  PRT13               PIC  9(01).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  PRT14               PIC  X(07).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT115-YY           PIC  99.
     02  P5                  PIC  X(01).
     02  PRT115-MM           PIC  99.
     02  P6                  PIC  X(01).
     02  PRT115-DD           PIC  99.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT116-YY           PIC  99.
     02  P7                  PIC  X(01).
     02  PRT116-MM           PIC  99.
     02  P8                  PIC  X(01).
     02  PRT116-DD           PIC  99.
****  明細行２               ****
 01  MEISAI2        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT150              PIC  Z.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
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
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT201              PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT202              PIC  N(20).
****  明細行３               ****
 01  MEISAI3        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  PRT21               PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT22               PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT23               PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT24               PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT241              PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT242              PIC  X(15).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT243              PIC  X(15).
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
 01  L-CNT                   PIC  9(3).
 01  P-CNT                   PIC  9(3).
 01  GYO-ZAN                 PIC  9(3).
 01  IN-CNT                  PIC  9(7)   VALUE  0.
 01  OT-CNT                  PIC  9(7)   VALUE  0.
 01  PG-CNT                  PIC  9(7)   VALUE  0.
****  フラグ                  ****
 01  PFK-FLG                 PIC  9(01)  VALUE  0.
 01  ERR-FLG                 PIC  9(01)  VALUE  0.
 01  MAIN-FLG                PIC  9(02)  VALUE  0.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  SOK-FLG                 PIC  9(01)  VALUE  1.
 01  HAC-FLG                 PIC  9(01)  VALUE  0.
 01  INV-FLG                 PIC  9(01)  VALUE  ZERO.
****  インデックス            ****
 01  IXA                     PIC  9(02).
 01  IX                      PIC  9(02).
 01  X                       PIC  9(02).
 01  Z                       PIC  9(02).
*
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました".
     03  MSG-NO02            PIC  N(20)  VALUE
            NC"倉庫コードが違います".
     03  MSG-NO03            PIC  N(20)  VALUE
            NC"開始が終了を越えています".
     03  MSG-NO04            PIC  N(20)  VALUE
            NC"年月日が違います".
     03  MSG-NO05            PIC  N(20)  VALUE
            NC"対象データはありません".
     03  MSG-NO06            PIC  N(20)  VALUE
            NC"対象データ抽出中です".
 01  TBL-MSG-R   REDEFINES    MSG-TBL.
     03  TBL-MSG             PIC  N(20)  OCCURS  6   TIMES.
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了".
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了　_項目戻り".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  2   TIMES.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0020L".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-SOKCD            PIC  X(02).
   01  LINK-DSOKCD           PIC  X(02).
***************************************************************
 PROCEDURE               DIVISION      USING    LINK-SOKCD
                                                LINK-DSOKCD.
***************************************************************
*
 DECLARATIVES.
 FILEERR-SEC0           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC11          SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACHEDF.
     MOVE   "HACHEDF "        TO    ERR-FL-ID.
     MOVE    HED-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC12          SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACMEIF.
     MOVE   "HACMEIF "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
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
 SHA0020L-START         SECTION.
     DISPLAY  "**  SHA0020L   START  **"   UPON  CONS.
*
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC     UNTIL   MAIN-FLG = 99.
     PERFORM       END-SEC.
*
     DISPLAY  "**  SHA0020L    END   **"   UPON  CONS.
     STOP     RUN.
 SHA0020L-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     HACHEDF  HACMEIF   JYOKEN    TOKMS
                        TENMS    ZSHIMS    ZSOKMS    HMEIMS.
     OPEN     OUTPUT    PRINTF.
     MOVE     55            TO        L-CNT.
     MOVE     55            TO        GYO-ZAN.
     MOVE     1             TO        P-CNT.
     ACCEPT   SYSYMD    FROM     DATE.
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
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY  H-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM    H-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD    H-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*特販部名称編集
     MOVE    SPACE               TO        JYO-REC.
     INITIALIZE                            JYO-REC.
     MOVE    "99"                TO        JYO-F01.
     MOVE    "BUMON"             TO        JYO-F02.
     READ    JYOKEN
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"   TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03            TO    HEN-TOKHAN
     END-READ.
     MOVE    HEN-TOKHAN-AREA      TO   H-TOKHAN.
*
     MOVE    P-CNT        TO      PAGE-SUU.
*初期画面表示へ
     MOVE  1              TO      MAIN-FLG.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理（全体）                        *
************************************************************
 MAIN-SEC               SECTION.
     EVALUATE  MAIN-FLG
*初期画面表示
         WHEN   1      PERFORM  DSP-INIT-SEC
*ＢＯＤＹ部入力
         WHEN   2      PERFORM  DSP-BODY-SEC
*確認入力
         WHEN   3      PERFORM  DSP-KAKU-SEC
*帳票処理
         WHEN   4      PERFORM  PRINT-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
**************************************************************
*                画面初期表示処理
**************************************************************
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE        TO  DSP-CONTROL.
     MOVE  SPACE        TO  DSP-FHA00201.
     PERFORM  OPT-CLR-SEC.
*倉庫名取得
     IF     LINK-DSOKCD  NOT =  "01"
       MOVE   LINK-SOKCD       TO  SOK-F01    DSP-SOKCD
       READ   ZSOKMS
         INVALID
           MOVE   SPACE        TO  DSP-SOKNM
         NOT  INVALID
           MOVE   SOK-F02      TO  DSP-SOKNM
       END-READ
     END-IF.
*開始・終了年月日
     MOVE  SYS-YY       TO  DSP-SYY  DSP-EYY.
     MOVE  SYS-MM       TO  DSP-SMM  DSP-EMM.
     MOVE  SYS-DD       TO  DSP-SDD  DSP-EDD.
*ＢＯＤＹ部入力へ
     MOVE  2            TO  MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
************************************************************
*                ＢＯＤＹ部入力処理
************************************************************
 DSP-BODY-SEC       SECTION.
*画面表示
     MOVE      1        TO  PFK-FLG.
     PERFORM   DSP-WRITE-SEC.
*倉庫ＣＤプロテクト
     IF    LINK-DSOKCD  NOT = "01"
       MOVE  "X"          TO  EDIT-STATUS  OF  DSP-SOKCD
     END-IF.
*画面入力
     MOVE      "BODY"   TO  DSP-GROUP.
     PERFORM   DSP-READ-SEC.
*ＰＦ判定
     EVALUATE  DSP-FUNC
          WHEN "E000"
                          PERFORM  CHK-BODY-SEC
                          IF    ERR-FLG = ZERO
                                MOVE    3    TO   MAIN-FLG
                          END-IF
          WHEN "F004"
                          MOVE  1      TO  MAIN-FLG
          WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
          WHEN "F006"
                          CONTINUE
          WHEN OTHER
                          MOVE  1      TO  ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
************************************************************
*                ＢＯＤＹ部入力チェック
************************************************************
 CHK-BODY-SEC   SECTION.
*倉庫ＣＤ
     IF  LINK-DSOKCD     =  "01"
         IF  DSP-SOKCD  =  SPACE
             MOVE   NC"全倉庫"       TO  DSP-SOKNM
         ELSE
             MOVE   DSP-SOKCD        TO  SOK-F01
             READ   ZSOKMS
               INVALID  KEY
                 IF  ERR-FLG  =  ZERO
                     MOVE   2        TO  ERR-FLG
                 END-IF
                 MOVE  "R"    TO  EDIT-OPTION  OF  DSP-SOKCD
                 MOVE  "C"    TO  EDIT-CURSOR  OF  DSP-SOKCD
                 MOVE  SPACE         TO  DSP-SOKNM
               NOT  INVALID  KEY
                 MOVE  SOK-F02       TO  DSP-SOKNM
             END-READ
     END-IF.
*開始年月日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     DSP-SYY             TO   WK-SYY.
     MOVE     DSP-SMM             TO   WK-SMM.
     MOVE     DSP-SDD             TO   WK-SDD.
     MOVE     WK-SYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET  =  ZERO
         MOVE    "3"              TO   LINK-IN-KBN
         MOVE    WK-SYMD          TO   LINK-IN-YMD6
         CALL    "SKYDTCKB"    USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE    LINK-OUT-YMD     TO   WK-SDATE
     ELSE
         MOVE    ZERO             TO   WK-SDATE
         IF  ERR-FLG  =  ZERO
              MOVE   4       TO  ERR-FLG
         END-IF
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SYY
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SYY
                                 EDIT-OPTION  OF  DSP-SMM
                                 EDIT-OPTION  OF  DSP-SDD
     END-IF.
*終了年月日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     DSP-EYY             TO   WK-EYY.
     MOVE     DSP-EMM             TO   WK-EMM.
     MOVE     DSP-EDD             TO   WK-EDD.
     MOVE     WK-EYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET  =  ZERO
         MOVE    "3"              TO   LINK-IN-KBN
         MOVE    WK-EYMD          TO   LINK-IN-YMD6
         CALL    "SKYDTCKB"    USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE    LINK-OUT-YMD     TO   WK-EDATE
     ELSE
         MOVE    ZERO             TO   WK-EDATE
         IF  ERR-FLG  =  ZERO
              MOVE   4       TO  ERR-FLG
         END-IF
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-EYY
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-EYY
                                 EDIT-OPTION  OF  DSP-EMM
                                 EDIT-OPTION  OF  DSP-EDD
     END-IF.
*開始・終了の関連チェック
     IF        WK-SDATE  NOT =  ZERO   AND
               WK-EDATE  NOT =  ZERO   AND
               WK-SDATE     >   WK-EDATE
         IF  ERR-FLG  =  ZERO
              MOVE   3       TO  ERR-FLG
         END-IF
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SYY
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SYY
                                 EDIT-OPTION  OF  DSP-SMM
                                 EDIT-OPTION  OF  DSP-SDD
                                 EDIT-OPTION  OF  DSP-EYY
                                 EDIT-OPTION  OF  DSP-EMM
                                 EDIT-OPTION  OF  DSP-EDD
     END-IF.
 CHK-BODY-EXIT.
     EXIT.
************************************************************
*                確認入力処理
************************************************************
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  2                     TO  PFK-FLG.
     PERFORM  DSP-WRITE-SEC.
*画面入力
     MOVE     "KAKU"            TO  DSP-GROUP.
     PERFORM  DSP-READ-SEC.
*ＰＦ判定
     EVALUATE  DSP-FUNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F004"
                          MOVE  1       TO   MAIN-FLG
         WHEN "F005"
                          MOVE  99      TO  MAIN-FLG
         WHEN "F006"
                          MOVE  2       TO   MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
************************************************************
*         確認入力チェック（対象データ存在ＣＨＫ）
************************************************************
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO      TO      ERR-FLG.
*発注Ｆスタート
     MOVE  ZERO              TO  HAC-FLG.
     MOVE  DSP-SOKCD         TO  HED-F17.
     MOVE  ZERO              TO  HED-F02.
     START HACHEDF   KEY >=      HED-F17
                                 HED-F02
         INVALID
            MOVE   9         TO  HAC-FLG
         NOT  INVALID
*発注Ｆリード
            PERFORM  HED-READ
     END-START.
*対象データ存在ＣＨＫ
     IF  HAC-FLG  =  9
         MOVE    5    TO    ERR-FLG
     END-IF.
*
     IF  ERR-FLG  =  ZERO
         MOVE    6    TO    ERR-FLG
         PERFORM  DSP-WRITE-SEC
         MOVE    4    TO    MAIN-FLG
     ELSE
*********MOVE    2    TO    MAIN-FLG
         MOVE    99   TO    MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
************************************************************
*               帳票出力
************************************************************
 PRINT-SEC              SECTION.
     INITIALIZE                  TBL-AREA.
     MOVE    ZERO           TO   Z.
*
     MOVE    SPACE          TO   MEISAI1
                                 MEISAI2
                                 MEISAI3
                                 MEISAI4.
* ヘッド情報の編集
     PERFORM   PRT-KYOTU.
* 明細情報の編集
     PERFORM   BDY-START.
     PERFORM   UNTIL  (NEW-KEY    NOT  =   OLD-KEY)
            ADD    1            TO   Z
            MOVE   BDY-F03      TO   HZ-GYO    (Z)
            MOVE   BDY-F06      TO   HZ-SYOCD  (Z)
            MOVE   BDY-F07      TO   HZ-HINTAN (Z)
            MOVE   BDY-F09      TO   HZ-SURYO  (Z)
            MOVE   BDY-F11      TO   HZ-TAN24  (Z)
            MOVE   BDY-F12      TO   HZ-SIRTAN (Z)
            MOVE   BDY-F13      TO   HZ-TAN26  (Z)
            MOVE   BDY-F14      TO   HZ-GENTAN (Z)
            MOVE   BDY-F15      TO   HZ-HANTAN (Z)
            MOVE   BDY-F16      TO   HZ-BIKO   (Z)
            PERFORM   BDY-READ
     END-PERFORM.
     IF  ( P-CNT    =    01       ) OR
         ( H1-SOKCD NOT = OLD-F17 ) OR
         ( GYO-ZAN  <     4 + Z   )
            PERFORM   MIDASI-SEC
     END-IF.
     PERFORM          PRT-SEC.
* 次の発注ファイル（ヘッド）読み込み
     PERFORM          HED-READ.
     IF   HAC-FLG  =  9
          MOVE    99        TO   MAIN-FLG
     END-IF.
 PRINT-END.
     EXIT.
************************************************************
*     2.1         プリント共通項目転送処理                 *
************************************************************
 PRT-KYOTU                  SECTION.
     IF  HED-F24    =   ZERO
         IF  HED-F99    =   ZERO    OR
             HED-F99    =   HED-F98
             MOVE      NC"登録"       TO   PRT01
         ELSE
             MOVE      NC"修正"       TO   PRT01
         END-IF
     ELSE
             MOVE      NC"削除"       TO   PRT01
     END-IF.
     MOVE    HED-F01             TO   PRT02.
     PERFORM      JYO-READ.
     IF   INV-FLG   =  ZERO
          MOVE    JYO-F03        TO   PRT03
     END-IF.
     MOVE    HED-F02             TO   PRT04.
     IF  HED-F10  NOT   =   ZERO
         MOVE     HED-F10(3:2)   TO   PRT05-YY
         MOVE     HED-F10(5:2)   TO   PRT05-MM
         MOVE     HED-F10(7:2)   TO   PRT05-DD
         MOVE     "."            TO   P1
                                      P2
     END-IF.
     IF  HED-F11  NOT   =   ZERO
         MOVE     HED-F11(3:2)   TO   PRT06-YY
         MOVE     HED-F11(5:2)   TO   PRT06-MM
         MOVE     HED-F11(7:2)   TO   PRT06-DD
         MOVE     "."            TO   P3
                                      P4
     END-IF.
     MOVE    HED-F05             TO   PRT07.
     IF  HED-F17  NOT =   ZERO
         MOVE    HED-F17         TO   PRT08
         MOVE    1               TO   SOK-FLG
         PERFORM      SOK-READ
         IF  INV-FLG  =   ZERO
             MOVE     SOK-F02    TO   PRT09
         END-IF
     END-IF.
     IF  HED-F09  NOT =   ZERO
         MOVE     HED-F09        TO   PRT10
     END-IF.
     MOVE    HED-F141            TO   PRT11.
     MOVE    HED-F142            TO   PRT12.
     MOVE    HED-F20             TO   PRT121.
     MOVE    HED-F13             TO   PRT13.
     MOVE    HED-F15             TO   PRT14.
     IF  HED-F98  NOT   =   ZERO
         MOVE     HED-F98(3:2)   TO   PRT115-YY
         MOVE     HED-F98(5:2)   TO   PRT115-MM
         MOVE     HED-F98(7:2)   TO   PRT115-DD
         MOVE     "."            TO   P5
                                      P6
     END-IF.
     IF  HED-F99  NOT   =   ZERO
         MOVE     HED-F99(3:2)   TO   PRT116-YY
         MOVE     HED-F99(5:2)   TO   PRT116-MM
         MOVE     HED-F99(7:2)   TO   PRT116-DD
         MOVE     "."            TO   P7
                                      P8
     END-IF.
*
     MOVE HED-F04  TO   PRT150.
     IF   HED-F06   NOT =  SPACE
          MOVE    HED-F06        TO   PRT15
          PERFORM      ZSH-READ
          IF  INV-FLG  =  ZERO
              MOVE     ZSH-F02   TO   PRT16
          ELSE
              MOVE     SPACE     TO   PRT16
          END-IF
     END-IF.
     IF   HED-F07   NOT =   ZERO
          MOVE    HED-F07             TO   PRT17
          PERFORM      TOK-READ
          IF  INV-FLG  =  ZERO
              MOVE     TOK-F02        TO   PRT18
          ELSE
              MOVE     SPACE          TO   PRT18
          END-IF
     END-IF.
     IF  HED-F01  =   70  OR  71  OR  80  OR  81
         IF  HED-F07(1:5)   =   99999
             IF  HED-F19    NOT =    "00"
                 MOVE       HED-F19   TO   PRT19
                 MOVE       2         TO   SOK-FLG
                 PERFORM    SOK-READ
                 IF  INV-FLG   =   ZERO
                     MOVE   SOK-F02   TO   PRT20
                 ELSE
                     MOVE   SPACE     TO   PRT20
                 END-IF
             END-IF
         ELSE
             IF  HED-F16    NOT =     "00000"
                 MOVE     HED-F16     TO   PRT19
                 PERFORM  TEN-READ
                 IF  INV-FLG  =  ZERO
                     MOVE   TEN-F02   TO   PRT20
                 ELSE
                     MOVE   SPACE     TO   PRT20
                 END-IF
             END-IF
         END-IF
     ELSE
         IF  HED-F16   NOT =     "00000"
             MOVE      HED-F16        TO   PRT19
             PERFORM   TEN-READ
             IF  INV-FLG  =  ZERO
                 MOVE  TEN-F02        TO   PRT20
             ELSE
                 MOVE  SPACE          TO   PRT20
             END-IF
         END-IF
     END-IF.
     MOVE    HED-F311       TO   PRT201.
     MOVE    HED-F312       TO   PRT202.
     MOVE    HED-F26        TO   PRT21.
     MOVE    HED-F27        TO   PRT22.
     MOVE    HED-F28        TO   PRT23.
     MOVE    HED-F29        TO   PRT24.
     IF  HED-F321   NOT =  ZERO
             MOVE    HED-F321    TO   PRT241
     END-IF.
     MOVE    HED-F322       TO   PRT242.
     MOVE    HED-F323       TO   PRT243.
 PRT-KYOTU-END.
     EXIT.
************************************************************
*     2.1.1       伝票区分名の取得                         *
************************************************************
 JYO-READ                        SECTION.
     MOVE    01             TO   JYO-F01.
     MOVE    HED-F01        TO   JYO-F02.
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
         MOVE     HED-F17        TO   SOK-F01
     ELSE
         MOVE     HED-F19        TO   SOK-F01
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
     MOVE    HED-F06         TO  ZSH-F01.
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
     MOVE    HED-F07         TO  TOK-F01.
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
     MOVE    HED-F07         TO  TEN-F52.
     MOVE    HED-F16         TO  TEN-F011.
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
     ADD     1              TO  PG-CNT.
     MOVE   PG-CNT          TO  PAGE-SUU.
*倉庫ＣＤ
     MOVE   OLD-F17         TO  SOK-F01    H1-SOKCD.
     READ   ZSOKMS
         INVALID  KEY
            MOVE   SPACE         TO  H1-SOKNM
         NOT INVALID
            MOVE   SOK-F02       TO  H1-SOKNM
     END-READ.
*
     IF      P-CNT    NOT =   1
             MOVE     SPACE   TO      P-REC
             WRITE    P-REC   AFTER   PAGE
             MOVE     55      TO      GYO-ZAN
     END-IF.
     WRITE   P-REC     FROM    MIDASI0     AFTER  2.
     WRITE   P-REC     FROM    MIDASI1     AFTER  1.
     WRITE   P-REC     FROM    MIDASI6     AFTER  1.
     WRITE   P-REC     FROM    MIDASI2     AFTER  1.
     WRITE   P-REC     FROM    MIDASI3     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
     WRITE   P-REC     FROM    MIDASI5     AFTER  1.
     WRITE   P-REC     FROM    MIDASI6     AFTER  1.
*
     ADD     1         TO      P-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*    2.3          プリント処理
************************************************************
 PRT-SEC                    SECTION.
     WRITE    P-REC         FROM    MEISAI1    AFTER  1.
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
*
     MOVE     SPACE         TO      P-REC.
     WRITE    P-REC         AFTER   1.
*
     COMPUTE      GYO-ZAN    =   GYO-ZAN  -  (4   +   X   -  1).
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
     CLOSE              DSPF     PRINTF
                        HACHEDF  HACMEIF   JYOKEN    TOKMS
                        TENMS    ZSHIMS    ZSOKMS    HMEIMS.
*
     DISPLAY "* HACMEIF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* HACMEIF (OT)=" OT-CNT   " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PG-CNT   " *" UPON CONS.
 END-END.
     EXIT.
************************************************************
*      発注ファイル（ヘッダ）の読込み
************************************************************
 HED-READ               SECTION.
     READ     HACHEDF
              AT  END
                  MOVE      9              TO  HAC-FLG
                  GO        TO             HED-READ-EXIT
     END-READ.
* 出力判定
     IF       DSP-SOKCD  NOT =  SPACE    AND
              DSP-SOKCD  NOT =  HED-F17
                  MOVE      9              TO  HAC-FLG
                  GO        TO             HED-READ-EXIT
     END-IF.
     IF     ( WK-SDATE <= HED-F98  AND  WK-EDATE >= HED-F98 ) OR
            ( WK-SDATE <= HED-F99  AND  WK-EDATE >= HED-F99 )
                  CONTINUE
     ELSE
                  GO        TO             HED-READ
     END-IF.
*
     MOVE    HED-F17        TO        OLD-F17.
     MOVE    HED-F02        TO        OLD-F02.
 HED-READ-EXIT.
     EXIT.
************************************************************
*         発注ファイル（明細）スタート
************************************************************
 BDY-START      SECTION.
     MOVE  OLD-KEY           TO  NEW-KEY.
     MOVE  HED-F02           TO  BDY-F02.
     MOVE  ZERO              TO  BDY-F03.
     START HACMEIF   KEY >=      BDY-F02
                                 BDY-F03
         INVALID
            MOVE  HIGH-VALUE     TO  NEW-KEY
         NOT  INVALID
            PERFORM  BDY-READ
     END-START.
 BDY-START-EXIT.
     EXIT.
************************************************************
*      発注ファイル（明細）の読込み
************************************************************
 BDY-READ               SECTION.
     READ     HACMEIF
              AT  END
                  MOVE      HIGH-VALUE     TO  NEW-KEY
                  GO        TO             BDY-READ-EXIT
     END-READ.
     ADD   1                TO  IN-CNT.
*
*    完了区分≠０の時は読み飛ばし
     IF    BDY-F05      NOT =     ZERO
           GO     TO    BDY-READ
     END-IF.
*
     MOVE  BDY-F02          TO  NEW-F02.
     IF    BDY-F02   =  HED-F02
           ADD    1         TO  OT-CNT
     END-IF.
 BDY-READ-EXIT.
     EXIT.
************************************************************
*      画面表示処理
************************************************************
 DSP-WRITE-SEC          SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)  TO   DSP-PFKEY.
*エラー設定
     IF  ERR-FLG        =    ZERO
         MOVE    SPACE       TO   DSP-ERRMSG
     ELSE
         MOVE    TBL-MSG(ERR-FLG)  TO   DSP-ERRMSG
         MOVE    ZERO        TO   ERR-FLG
     END-IF.
*
     MOVE     HEN-DATE       TO   DSP-SDATE.
     MOVE     HEN-TIME       TO   DSP-STIME.
     MOVE     HEN-TOKHAN-AREA     TO   DSP-TOKHAN.
*
     MOVE    "FHA00201"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     WRITE    DSP-FHA00201.
 DSP-WRITE-END.
     EXIT.
************************************************************
*      画面データの入力処理
************************************************************
 DSP-READ-SEC           SECTION.
     MOVE  "NE"      TO   DSP-PROC.
     READ   DSPF.
*
     PERFORM       OPT-CLR-SEC.
 DSP-READ-END.
     EXIT.
*************************************************************
*      項目属性初期化
*************************************************************
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-SOKCD
                             EDIT-CURSOR  OF  DSP-SYY
                             EDIT-CURSOR  OF  DSP-SMM
                             EDIT-CURSOR  OF  DSP-SDD
                             EDIT-CURSOR  OF  DSP-EYY
                             EDIT-CURSOR  OF  DSP-EMM
                             EDIT-CURSOR  OF  DSP-EDD.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-SOKCD
                             EDIT-OPTION  OF  DSP-SYY
                             EDIT-OPTION  OF  DSP-SMM
                             EDIT-OPTION  OF  DSP-SDD
                             EDIT-OPTION  OF  DSP-EYY
                             EDIT-OPTION  OF  DSP-EMM
                             EDIT-OPTION  OF  DSP-EDD.
 OPT-CLR-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
