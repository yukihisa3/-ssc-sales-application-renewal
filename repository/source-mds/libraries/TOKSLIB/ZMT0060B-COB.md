# ZMT0060B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZMT0060B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　倉庫マスタリスト                  *
*    作成日／更新日　　　：　93/04/14                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　倉庫マスタリストの出力を行う      *
*    更新日　　　　　　　：　97/03/11                          *
*    　　　　　　　　　　：　運転手段の追加　　　　　　　      *
*    更新日　　　　　　　：　12/07/24                          *
*    　　　　　　　　　　：　出荷送信（オ）出荷送信（手）      *
*                            小売連携区分　追加                *
*    更新日　　　　　　　：　12/10/09                          *
*                            物流連携区分　追加                *
*    更新日　　　　　　　：　20/02/12                          *
*                            倉庫区分　　　追加                *
*    更新日　　　　　　　：　20/05/14                          *
*                            Ｄ３６５用項目追加　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMT0060B.
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
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
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
****<< 倉庫マスタファイル >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   SOK01
                        FILE    STATUS       IS   SOK-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     ZMT0060           OF   XMDLIB.
****<< 倉庫マスタファイル >>******************************
 FD    ZSOKMS    BLOCK   CONTAINS   2   RECORDS.
 01  SOUKO-R.
     02 SOK01                PIC  X(02).
     02 SOK02                PIC  N(18).
     02 SOK03                PIC  X(25).
     02 SOK04                PIC  X(05).
     02 SOK05                PIC  N(18).
     02 SOK06                PIC  N(18).
     02 SOK07                PIC  X(15).
     02 SOK08                PIC  X(15).
     02 SOK09                PIC  9(02).
***  97.03.11 ST  ***
     02 SOK10                PIC  X(02).
*    02 FILLER               PIC  X(28).
*****02 FILLER               PIC  X(26).
***  97.03.11 ED  ***
*****2012/07/24 UPDATE ST
     02 SOK111               PIC  X(03).
     02 SOK112               PIC  X(04).
     02 SOK12                PIC  X(01).
*****2012/10/09 UPDATE ST
*****02 FILLER               PIC  X(16).
     02 SOK13                PIC  X(01).
*****2020/02/12 UPDATE ST
*****02 FILLER               PIC  X(15).
     02 SOK14                PIC  X(01).
*# 2020/05/14 NAV ST
     02 SOK15                PIC  X(02).
     02 SOK16                PIC  X(03).
     02 SOK17                PIC  X(02).
*****02 FILLER               PIC  X(14).
     02 FILLER               PIC  X(07).
*# 2020/05/14 NAV ED
*****2012/10/09 UPDATE ED
     02 SOK98                PIC  X(01).
     02 SOK99                PIC  X(01).
*****2012/07/24 UPDATE ED
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(2).
     02 DSP-GROUP            PIC  X(8).
     02 DSP-FORMAT           PIC  X(8).
     02 DSP-STATUS           PIC  X(2).
     02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 SOK-STATUS           PIC  X(2).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
****  日付保存                ****
*01  SYSTEM-HIZUKE.
*    02  SYSYMD              PIC  9(6).
*    02  SYSYMD-R            REDEFINES SYSYMD.
*      03  SYS-YY            PIC  99.
*      03  SYS-MM            PIC  99.
*      03  SYS-DD            PIC  99.
*日付／時刻
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
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(34)  VALUE  SPACE.
     02  FILLER              PIC  N(16)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　倉庫マスタリスト　＊＊＊".
     02  FILLER              PIC  X(18)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  99.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZ9.
     02  FILLER              PIC  X(8)   VALUE  SPACE.
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(10)  VALUE
         NC"倉庫　倉庫名（漢字）".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"_".
*****2012/07/24 UPDATE ST
*****02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
*****2012/07/24 UPDATE ED
     02  FILLER              PIC  N(05)  VALUE
         NC"住　所　１".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"住　所　２".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"ＴＥＬ".
***  97.03.11 ST  ***
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"運転".
***  97.03.11 ED  ***
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(18)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"（カナ）".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"県コード".
*****2012/07/24 UPDATE ST
*****02  FILLER              PIC  X(61)  VALUE  SPACE.
     02  FILLER              PIC  X(64)  VALUE  SPACE.
*****2012/07/24 UPDATE ED
     02  FILLER              PIC  N(03)  VALUE
         NC"ＦＡＸ".
***  97.03.11 ST  ***
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"手段".
***  97.03.11 ED  ***
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  SOK-01              PIC  X(02).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  SOK-02              PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
*****2012/07/24 UPDATE ST
*****02  SOK-04              PIC  X(05).
*****02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SOK-04              PIC  X(08).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
*****2012/07/24 UPDATE ED
     02  SOK-05              PIC  N(18).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  SOK-06              PIC  N(18).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  SOK-07              PIC  X(15).
****  明細行２               ****
 01  MEISAI-2.
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  SOK-03              PIC  X(25).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  SOK-09              PIC  9(02).
*****2012/07/24 UPDATE ST
*****02  FILLER              PIC  X(67)  VALUE  SPACE.
     02  FILLER              PIC  X(70)  VALUE  SPACE.
*****2012/07/24 UPDATE ED
     02  SOK-08              PIC  X(15).
***  97.03.11 ST  ***
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SOK-10              PIC  X(02).
***  97.03.11 ED  ***
*****2012/07/24 ADD    ST
****  明細行３               ****
*#2020/05/14 NAV ST
*01  MEISAI-3       CHARACTER     TYPE   IS   NIHONGO.
 01  MEISAI-3       CHARACTER     TYPE   IS   YB.
*#2020/05/14 NAV ED
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE NC"出荷（オ）：".
     02  SOK-11              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SOK-12              PIC  N(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE NC"出荷（手）：".
     02  SOK-13              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SOK-14              PIC  N(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE NC"小売連携：".
     02  SOK-15              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SOK-16              PIC  N(02).
*****2012/10/09 UPDATE ST
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE NC"物流連携：".
     02  SOK-17              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SOK-18              PIC  N(02).
*****2012/10/09 UPDATE ED
*****2020/02/12 UPDATE ST
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE NC"倉庫区分：".
     02  SOK-19              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SOK-20              PIC  N(07).
*****2020/02/12 UPDATE ED
*****2020/05/14 UPDATE ST
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(04)  VALUE  "D365".
     02  FILLER              PIC  N(04)  VALUE  NC"連携用→".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(03)  VALUE  "ｻｲﾄ".
     02  FILLER              PIC  N(01)  VALUE  NC"：".
     02  SOK-21              PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"場所：".
     02  SOK-22              PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"保管：".
     02  SOK-23              PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
*****2020/02/12 UPDATE ED
*****2012/07/24 ADD    ED
**** 線１
 01  SEN1.
     02  WK-SEN1             OCCURS  136.
         03  FILLER          PIC  X(01)  VALUE  "=".
**** 線２
 01  SEN2.
     02  WK-SEN2             OCCURS  136.
         03  FILLER          PIC  X(01)  VALUE  "-".
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMT0060B".
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
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
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
 ZMT0060B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     IF   PG-END   NOT =  "END"
          MOVE     SPACE   TO   END-FLG
          GO  TO   ZMT0060B-START
     END-IF.
     STOP     RUN.
 ZMT0060B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
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
     OPEN     I-O       DSPF.
     OPEN     INPUT     ZSOKMS.
     OPEN     OUTPUT    PRINTF.
     MOVE    "ZMT0060"  TO     DSP-FORMAT.
     MOVE     SPACE     TO     ZMT0060.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     MOVE     62        TO     L-CNT.
     MOVE     1         TO     P-CNT.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     SCODE     TO     SOK01.
     START    ZSOKMS    KEY  IS  >=  SOK01
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
     READ     ZSOKMS
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     IF       SOK01     >    ECODE
              MOVE  "END"    TO   END-FLG
     END-IF.
*****ACCEPT   SYSYMD    FROM     DATE.
 INIT-END.
     EXIT.
************************************************************
*      _１     画面処理                                   *
************************************************************
 DSP-SEC                SECTION.
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "GPALL "   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     WRITE    ZMT0060.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     IF   DSP-FLG   =   "0"
        MOVE    "GPHEAD"   TO     DSP-GROUP
     ELSE
        MOVE    "KAKNIN"   TO     DSP-GROUP
     END-IF.
     READ     DSPF.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                IF   DSP-FLG   =   "1"
                     MOVE   "0"       TO  DSP-FLG
                     MOVE   SPACE     TO  HEAD
                ELSE
                     MOVE   SPACE     TO  HEAD
                     MOVE   MSG-ERR1  TO  MSG1
                END-IF
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
                IF   SCODE  =    SPACE
                     MOVE   "  "  TO  SCODE
                END-IF
                IF   ECODE  =    SPACE
                     MOVE   "99"   TO  ECODE
                END-IF
                IF   SCODE >   ECODE
                     MOVE   MSG-ERR2  TO  MSG1
                     GO TO  DSP-SEC
                END-IF
                IF   DSP-FLG   =   "0"
                     MOVE   "1"       TO  DSP-FLG
                     GO TO  DSP-SEC
                END-IF
         WHEN   OTHER
                MOVE   MSG-ERR1  TO  MSG1
                GO TO  DSP-SEC
     END-EVALUATE.
 DSP-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     IF       L-CNT         >=      62
              PERFORM       MIDASI-SEC
     END-IF.
     MOVE     SOK01         TO      SOK-01.
     MOVE     SOK02         TO      SOK-02.
     MOVE     SOK03         TO      SOK-03.
*****2012/07/24 UPDATE ST
*****MOVE     SOK04         TO      SOK-04.
     MOVE     SOK111        TO      SOK-04(1:3).
     MOVE     "-"           TO      SOK-04(4:1).
     MOVE     SOK112        TO      SOK-04(5:4).
*****2012/07/24 UPDATE ED
     MOVE     SOK05         TO      SOK-05.
     MOVE     SOK06         TO      SOK-06.
     MOVE     SOK07         TO      SOK-07.
     MOVE     SOK08         TO      SOK-08.
     MOVE     SOK09         TO      SOK-09.
***  95.03.11 ST  ***
     MOVE     SOK10         TO      SOK-10.
***  95.03.11 ED  ***
*****2012/07/24 ADD    ST
     MOVE     SOK99         TO      SOK-11.
     EVALUATE SOK99
         WHEN SPACE  MOVE NC"無"    TO    SOK-12
         WHEN "1"    MOVE NC"有"    TO    SOK-12
         WHEN OTHER  MOVE NC"＊"    TO    SOK-12
     END-EVALUATE.
     MOVE     SOK98         TO      SOK-13.
     EVALUATE SOK98
         WHEN SPACE  MOVE NC"無"    TO    SOK-14
         WHEN "1"    MOVE NC"有"    TO    SOK-14
         WHEN OTHER  MOVE NC"＊"    TO    SOK-14
     END-EVALUATE.
     MOVE     SOK12         TO      SOK-15.
     EVALUATE SOK12
         WHEN SPACE  MOVE NC"無"    TO    SOK-16
         WHEN "1"    MOVE NC"有"    TO    SOK-16
         WHEN OTHER  MOVE NC"＊"    TO    SOK-16
     END-EVALUATE.
*****2012/07/24 ADD    ED
*****2012/10/09 ADD    ST
     MOVE     SOK13         TO      SOK-17.
     EVALUATE SOK13
         WHEN SPACE  MOVE NC"無"    TO    SOK-18
         WHEN "1"    MOVE NC"有"    TO    SOK-18
         WHEN OTHER  MOVE NC"＊"    TO    SOK-18
     END-EVALUATE.
*****2012/10/09 ADD    ED
*****2020/02/12 ADD    ST
     MOVE     SOK14         TO      SOK-19.
     EVALUATE SOK14
         WHEN SPACE  MOVE NC"ＮＡＶＳ倉庫"    TO    SOK-20
         WHEN "1"    MOVE NC"ＳＬＩＭＳ倉庫"  TO    SOK-20
         WHEN OTHER  MOVE ALL NC"＊"          TO    SOK-20
     END-EVALUATE.
*****2020/02/12 ADD    ED
*#2020/05/14 NAV ST
     MOVE     SOK15         TO      SOK-21.
     MOVE     SOK16         TO      SOK-22.
     MOVE     SOK17         TO      SOK-23.
*#2020/05/14 NAV ED
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
     WRITE    P-REC         FROM    MEISAI-2    AFTER  1.
     WRITE    P-REC         FROM    MEISAI-3    AFTER  1.
     WRITE    P-REC         FROM    SEN2        AFTER  1.
     ADD      4             TO      L-CNT.
     READ     ZSOKMS
              AT END   MOVE  "END"  TO  END-FLG
     END-READ.
     IF       SOK01         >       ECODE
                       MOVE  "END"  TO  END-FLG
     END-IF.
 MAIN-END.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE     WK-Y       TO      H-YY.
     MOVE     WK-M       TO      H-MM.
     MOVE     WK-D       TO      H-DD.
     MOVE     P-CNT      TO      PAGE-SUU.
     IF       P-CNT  = 1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    SEN1       AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
        ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    SEN1       AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
     END-IF.
     ADD      1  TO      P-CNT.
     MOVE     7  TO      L-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   PRINTF   ZSOKMS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
