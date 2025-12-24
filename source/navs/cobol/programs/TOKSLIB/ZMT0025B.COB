****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　商品在庫マスタリスト              *
*    作成日／更新日　　　：　93/06/25                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品在庫マスタリストの出力を行う　*
*                            （_番順）                        *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMT0025B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K6500.
 OBJECT-COMPUTER.       K6500.
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
     SELECT   DSPF      ASSIGN              TO   GS-DSPF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        SYMBOLIC DESTINATION IS  "DSP"
                        PROCESSING MODE      IS   DSP-PROC
                        GROUP                IS   DSP-GROUP
                        FORMAT               IS   DSP-FORMAT
                        SELECTED FUNCTION    IS   DSP-FUNC
                        DESTINATION-1        IS   DSP-WSNO
                        FILE STATUS          IS   DSP-STATUS.
****<< 商品在庫マスタファイル >>****************************
     SELECT   ZZAIMS    ASSIGN    TO        DA-01-VI-ZZAIMS4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F031
                                                 ZAI-F032
                                                 ZAI-F033
                                                 ZAI-F021
                                                 ZAI-F022
                        FILE      STATUS    IS   ZAI-STATUS.
****<< 倉庫マスタファイル >>********************************
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
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
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     ZMT0025       OF   XMDLIB.
****<< 商品在庫マスタファイル >>****************************
 FD  ZZAIMS.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZAI       PREFIX.
****<< 倉庫マスタファイル >>********************************
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
****<< 商品名称マスタファイル >>****************************
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****<< 条件ファイル　　　　　 >>****************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
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
     02 DSP-WSNO             PIC  X(8).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
 01  INVALID-FLG             PIC  9(01)  VALUE ZERO.
****  インデックス            ****
 01  IXA                     PIC  9(02).
 01  IXB                     PIC  9(02).
****  品単右詰め用エリア      ****
 01  HINTAN                     PIC X(05).
 01  WK-MIGI.
     03  WK-CNT1                PIC 9(01).
     03  WK-CNT2                PIC 9(01).
     03  MIG-SW                 PIC 9(01).
     03  WK1-HIN                PIC X(05).
     03  WK1-HIN1               REDEFINES   WK1-HIN.
         05  WK1-HIN2               PIC X(01)  OCCURS 5.
     03  WK2-HIN                PIC X(05).
     03  WK2-HIN1               REDEFINES   WK2-HIN.
         05  WK2-HIN2               PIC X(01)  OCCURS 5.
****  ワークエリア            ****
 01  WRK-AREA.
     02  WRK-KEY1.
         03  WRK-KEY11       PIC  X(02).
         03  WRK-KEY12       PIC  X(06).
         03  WRK-KEY13       PIC  X(08).
         03  WRK-KEY14       PIC  X(08).
     02  WRK-KEY2.
         03  WRK-KEY21       PIC  X(02).
         03  WRK-KEY22       PIC  X(06).
         03  WRK-KEY23       PIC  X(08).
         03  WRK-KEY24       PIC  X(08).
     02  WRK-KEY3.
         03  WRK-KEY31       PIC  X(02).
         03  WRK-KEY32       PIC  X(06).
         03  WRK-KEY33       PIC  X(08).
         03  WRK-KEY34       PIC  X(08).
     02  WSHTAN.
         03  WSHTAN1         PIC  X(05).
         03  WSHTAN2         PIC  X(02).
         03  WSHTAN3         PIC  X(01).
     02  WEHTAN.
         03  WEHTAN1         PIC  X(05).
         03  WEHTAN2         PIC  X(02).
         03  WEHTAN3         PIC  X(01).
     02  WSTANA.
         03  WSTANA1         PIC  X(01).
         03  WSTANA2         PIC  X(03).
         03  WSTANA3         PIC  X(02).
     02  WETANA.
         03  WETANA1         PIC  X(01).
         03  WETANA2         PIC  X(03).
         03  WETANA3         PIC  X(02).
     02  WSHONM.
         03  WSHONM1         PIC  N(15).
         03  WSHONM2         PIC  N(15).
 01   WK-ZAIKOSU             PIC S9(09)V99      VALUE ZERO.
 01   WK-SOKCD               PIC  9(09)V99.
 01   WK-SOKO     REDEFINES  WK-SOKCD.
      03  WK-SOKO1           PIC  9(07).
      03  WK-SOKO2           PIC  9(02).
      03  WK-SOK03           PIC  9(02).
 01   WK-SOKO2R              PIC  X(02).
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(6).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(34)  VALUE  SPACE.
     02  FILLER              PIC  N(18)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　商品在庫マスタリスト　＊＊＊".
     02  FILLER              PIC  X(12)  VALUE  SPACE.
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
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"倉庫".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"_　番".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"商品".
     02  FILLER              PIC  X(04)  VALUE  "ｺｰﾄﾞ".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"品　単".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"カナ名".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"現在庫数".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"当月発注数".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"当月入庫数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"未入庫数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"引当済数".
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"取引先".
     02  FILLER              PIC  X(02)  VALUE  "CD".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"商　品　名".
*****02  FILLER              PIC  X(50)  VALUE  SPACE.
     02  FILLER              PIC  X(37)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"引当済在庫".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"当月入出庫数".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"当月出庫数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"未出庫数".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"前月未在庫数".
****  見出し行４             ****
 01  MIDASI-4       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"当年度実績".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注数".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"入庫数".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"出庫数".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"前年度実績".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注数".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"入庫数".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"出庫数".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-01              PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SOKNM               PIC  N(06).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-031             PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  ZAI-032             PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  ZAI-033             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-021             PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-0221            PIC  X(05).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-0222            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-0223            PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-04              PIC  X(15).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  ZAI-06              PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-09              PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-10              PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-12              PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-14              PIC  ---,---,--9.99.
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  ZAI-05              PIC  9(08).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SHONM               PIC  N(30).
**** 02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-06-13           PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-08              PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-11              PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-13              PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-07              PIC  ---,---,--9.99.
****  明細行３               ****
 01  MEISAI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(15)  VALUE  SPACE.
     02  TUKI1               PIC  X(02).
     02  FILLER              PIC  N(01)  VALUE  NC"月".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-161             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-162             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-163             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  TUKI2               PIC  X(02).
     02  FILLER              PIC  N(01)  VALUE  NC"月".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-151             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-152             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZAI-153             PIC  ---,---,--9.99.
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  MSG-ERR3            PIC  N(30)  VALUE
            NC"実績出力指示エラー".
     02  MSG-ERR4            PIC  N(30)  VALUE
            NC"他倉庫は指示できません".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMT0025B".
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
                   PROCEDURE  ZZAIMS.
     MOVE   "ZZAIMS  "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
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
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 ZMT0025B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     IF   PG-END   NOT =  "END"
          MOVE     SPACE   TO   END-FLG
          GO  TO   ZMT0025B-START
     END-IF.
     STOP     RUN.
 ZMT0025B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     ZZAIMS    ZSOKMS    HMEIMS
                        HJYOKEN.
     OPEN     OUTPUT    PRINTF.
*
     MOVE    "ZMT0025"  TO     DSP-FORMAT.
     MOVE     SPACE     TO     ZMT0025.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     MOVE     62        TO     L-CNT.
     MOVE     1         TO     P-CNT.
*
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
*
     MOVE     SSOKCD    TO     ZAI-F01.
     MOVE     STANA1    TO     ZAI-F03(1:1).
     MOVE     STANA2    TO     ZAI-F03(2:3).
     MOVE     STANA3    TO     ZAI-F03(5:2).
     MOVE     SSHOCD    TO     ZAI-F021.
     MOVE     WSHTAN    TO     ZAI-F022.
     START    ZZAIMS    KEY  IS  >=  ZAI-F01
                                     ZAI-F031 ZAI-F032 ZAI-F033
                                     ZAI-F021 ZAI-F022
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-START.
     READ     ZZAIMS
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     MOVE     ZAI-F01   TO     WRK-KEY31.
     MOVE     ZAI-F03   TO     WRK-KEY32.
     MOVE     ZAI-F021  TO     WRK-KEY33.
     MOVE     ZAI-F022  TO     WRK-KEY34.
     IF       WRK-KEY3  >    WRK-KEY2
              MOVE  "END"    TO   END-FLG
     END-IF.
     ACCEPT   SYSYMD    FROM     DATE.
 INIT-END.
     EXIT.
************************************************************
*      _１     画面処理                                   *
************************************************************
 DSP-SEC                SECTION.
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "GPALL "   TO     DSP-GROUP.
     WRITE    ZMT0025.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     IF   DSP-FLG   =   "0"
        MOVE    "GPHEAD"   TO     DSP-GROUP
     ELSE
        MOVE    "KAKNIN"   TO     DSP-GROUP
     END-IF.
     READ     DSPF.
     MOVE     65             TO   JYO-F01.
     MOVE     DSP-WSNO       TO   JYO-F02.
     READ     HJYOKEN.
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
                IF   SSOKCD =    SPACE
                     MOVE   "  "         TO  SSOKCD
                END-IF
                IF   ESOKCD =    SPACE
                     MOVE   "99"         TO  ESOKCD
                END-IF
                IF   SSHOCD =    SPACE
                     MOVE   "        "   TO  SSHOCD
                END-IF
                IF   STANA1 =    SPACE
                     MOVE   " "          TO  STANA1
                END-IF
                IF   ETANA1 =    SPACE
                     MOVE   "9"          TO  ETANA1
                END-IF
                IF   STANA2 =    SPACE
                     MOVE   "   "        TO  STANA2
                END-IF
                IF   ETANA2 =    SPACE
                     MOVE   "999"        TO  ETANA2
                END-IF
                IF   STANA3 =    SPACE
                     MOVE   "  "         TO  STANA3
                END-IF
                IF   ETANA3 =    SPACE
                     MOVE   "99"         TO  ETANA3
                END-IF
                IF   ESHOCD =    SPACE
                     MOVE   "99999999"   TO  ESHOCD
                END-IF
                IF   SHTAN1 =    SPACE
                     MOVE   "     "      TO  SHTAN1
                END-IF
                IF   EHTAN1 =    SPACE
                     MOVE   "99999"      TO  EHTAN1
                END-IF
                IF   SHTAN2 =    SPACE
                     MOVE   "  "         TO  SHTAN2
                END-IF
                IF   EHTAN2 =    SPACE
                     MOVE   "99"         TO  EHTAN2
                END-IF
                IF   SHTAN3 =    SPACE
                     MOVE   " "          TO  SHTAN3
                END-IF
                IF   EHTAN3 =    SPACE
                     MOVE   "9"          TO  EHTAN3
                END-IF
                IF  (ZOUTF  NOT = SPACE) AND
                    (ZOUTF  NOT = "Y")
                     MOVE   MSG-ERR3  TO  MSG1
                     GO TO  DSP-SEC
                END-IF
                MOVE     SHTAN1    TO     HINTAN
                IF  HINTAN  NOT  =  SPACE
                    PERFORM  HINTAN-SEC
                END-IF
                MOVE     HINTAN    TO     SHTAN1
                MOVE     SHTAN1    TO     WSHTAN1
                MOVE     SHTAN2    TO     WSHTAN2
                MOVE     SHTAN3    TO     WSHTAN3
                MOVE     EHTAN1    TO     HINTAN
                IF  HINTAN  NOT  =  SPACE
                    PERFORM  HINTAN-SEC
                END-IF
                MOVE     HINTAN    TO     EHTAN1
                MOVE     EHTAN1    TO     WEHTAN1
                MOVE     EHTAN2    TO     WEHTAN2
                MOVE     EHTAN3    TO     WEHTAN3
                MOVE     STANA1    TO     WSTANA1
                MOVE     STANA2    TO     WSTANA2
                MOVE     STANA3    TO     WSTANA3
                MOVE     ETANA1    TO     WETANA1
                MOVE     ETANA2    TO     WETANA2
                MOVE     ETANA3    TO     WETANA3
                MOVE     SSOKCD    TO     WRK-KEY11
                MOVE     WSTANA    TO     WRK-KEY12
                MOVE     SSHOCD    TO     WRK-KEY13
                MOVE     WSHTAN    TO     WRK-KEY14
                MOVE     ESOKCD    TO     WRK-KEY21
                MOVE     WETANA    TO     WRK-KEY22
                MOVE     ESHOCD    TO     WRK-KEY23
                MOVE     WEHTAN    TO     WRK-KEY24
                IF   WRK-KEY1 >   WRK-KEY2
                     MOVE   MSG-ERR2  TO  MSG1
                     GO TO  DSP-SEC
                END-IF
*\\
                MOVE JYO-F04   TO         WK-SOKCD
                MOVE WK-SOKO2  TO         WK-SOKO2R
                IF   WK-SOKO2R NOT =      "01"
                  IF SSOKCD    NOT =      WK-SOKO2R
                     MOVE   MSG-ERR4  TO  MSG1
                     GO TO  DSP-SEC
                  ELSE
                      IF ESOKCD    NOT =      WK-SOKO2R
                         MOVE   MSG-ERR4  TO  MSG1
                         GO TO  DSP-SEC
                      END-IF
                  END-IF
                END-IF
*
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
*      1.1.1      品単右づめ処理                           *
************************************************************
 HINTAN-SEC            SECTION.
     MOVE     SPACE          TO        WK1-HIN   WK2-HIN.
     MOVE     HINTAN         TO        WK1-HIN.
     MOVE     0              TO        MIG-SW.
     PERFORM     VARYING   IXA       FROM    5  BY  -1
                 UNTIL     IXA <  1   OR  MIG-SW = 1
         IF  ( WK1-HIN2(IXA) NOT  =  SPACE  AND  MIG-SW = 0 )
             MOVE    IXA    TO    WK-CNT1
             MOVE    1      TO    MIG-SW
         END-IF
     END-PERFORM.
     IF   WK-CNT1  =  5
          GO  TO  HINTAN-EXIT
     END-IF.
     COMPUTE   WK-CNT2  =  5 - WK-CNT1.
     PERFORM     VARYING   IXB       FROM    1  BY  1
                 UNTIL     IXB >  WK-CNT1
         ADD       1                TO    WK-CNT2
         MOVE      WK1-HIN2(IXB)    TO    WK2-HIN2(WK-CNT2)
     END-PERFORM.
     MOVE     WK2-HIN        TO        HINTAN.
 HINTAN-EXIT.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     IF       ZOUTF  =  "Y"
         IF       L-CNT         >=      54
                  PERFORM       MIDASI-SEC
         END-IF
     ELSE
         IF       L-CNT         >=      62
                  PERFORM       MIDASI-SEC
         END-IF
     END-IF.
     PERFORM     SOK-READ-SEC.
     PERFORM     SHO-READ-SEC.
     MOVE     ZAI-F01       TO      ZAI-01.
     MOVE     ZAI-F021      TO      ZAI-021.
     MOVE     ZAI-F022(1:5) TO      ZAI-0221.
     MOVE     ZAI-F022(6:2) TO      ZAI-0222.
     MOVE     ZAI-F022(8:1) TO      ZAI-0223.
     MOVE     ZAI-F03(1:1)  TO      ZAI-031.
     MOVE     ZAI-F03(2:3)  TO      ZAI-032.
     MOVE     ZAI-F03(5:2)  TO      ZAI-033.
     MOVE     ZAI-F04       TO      ZAI-04.
     MOVE     ZAI-F05       TO      ZAI-05.
     MOVE     ZAI-F06       TO      ZAI-06.
     MOVE     ZAI-F07       TO      ZAI-07.
     MOVE     ZAI-F08       TO      ZAI-08.
     COMPUTE  WK-ZAIKOSU    =       ZAI-F06 - ZAI-F13.
     MOVE     WK-ZAIKOSU    TO      ZAI-06-13.
     MOVE     ZAI-F09       TO      ZAI-09.
     MOVE     ZAI-F10       TO      ZAI-10.
     MOVE     ZAI-F11       TO      ZAI-11.
     MOVE     ZAI-F12       TO      ZAI-12.
     MOVE     ZAI-F13       TO      ZAI-13.
     MOVE     ZAI-F14       TO      ZAI-14.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  2.
     WRITE    P-REC         FROM    MEISAI-2    AFTER  1.
     IF       ZOUTF  =  "Y"
         MOVE       SPACE   TO      P-REC
         WRITE      P-REC   AFTER   1
         PERFORM     VARYING   IXA       FROM    1  BY  1
                     UNTIL     IXA  >  12
             EVALUATE    IXA
                WHEN   1
                        MOVE   " 6"        TO   TUKI1  TUKI2
                WHEN   2
                        MOVE   " 7"        TO   TUKI1  TUKI2
                WHEN   3
                        MOVE   " 8"        TO   TUKI1  TUKI2
                WHEN   4
                        MOVE   " 9"        TO   TUKI1  TUKI2
                WHEN   5
                        MOVE   "10"        TO   TUKI1  TUKI2
                WHEN   6
                        MOVE   "11"        TO   TUKI1  TUKI2
                WHEN   7
                        MOVE   "12"        TO   TUKI1  TUKI2
                WHEN   8
                        MOVE   " 1"        TO   TUKI1  TUKI2
                WHEN   9
                        MOVE   " 2"        TO   TUKI1  TUKI2
                WHEN  10
                        MOVE   " 3"        TO   TUKI1  TUKI2
                WHEN  11
                        MOVE   " 4"        TO   TUKI1  TUKI2
                WHEN  12
                        MOVE   " 5"        TO   TUKI1  TUKI2
             END-EVALUATE
             MOVE    ZAI-F151(IXA)    TO   ZAI-151
             MOVE    ZAI-F152(IXA)    TO   ZAI-152
             MOVE    ZAI-F153(IXA)    TO   ZAI-153
             MOVE    ZAI-F161(IXA)    TO   ZAI-161
             MOVE    ZAI-F162(IXA)    TO   ZAI-162
             MOVE    ZAI-F163(IXA)    TO   ZAI-163
             WRITE    P-REC   FROM    MEISAI-3    AFTER  1
         END-PERFORM
         ADD      16            TO      L-CNT
     ELSE
         ADD      3             TO      L-CNT
     END-IF.
     READ     ZZAIMS
              AT END   MOVE  "END"  TO  END-FLG
     END-READ.
     MOVE     ZAI-F01   TO     WRK-KEY31.
     MOVE     ZAI-F03   TO     WRK-KEY32.
     MOVE     ZAI-F021  TO     WRK-KEY33.
     MOVE     ZAI-F022  TO     WRK-KEY34.
     IF       WRK-KEY3  >    WRK-KEY2
              MOVE  "END"    TO   END-FLG
     END-IF.
 MAIN-END.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE     SYS-YY     TO      H-YY.
     MOVE     SYS-MM     TO      H-MM.
     MOVE     SYS-DD     TO      H-DD.
     MOVE     P-CNT      TO      PAGE-SUU.
     IF       P-CNT  = 1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              IF       ZOUTF  =  "Y"
                  WRITE   P-REC   FROM   MIDASI-4   AFTER  1
              END-IF
        ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              IF       ZOUTF  =  "Y"
                  WRITE   P-REC   FROM   MIDASI-4   AFTER  1
              END-IF
     END-IF.
     ADD      1  TO      P-CNT.
     IF       ZOUTF  =  "Y"
              MOVE     6  TO      L-CNT
     ELSE
              MOVE     5  TO      L-CNT
     END-IF.
 MIDASI-END.
     EXIT.
************************************************************
*      2.2       倉庫名の取得                              *
************************************************************
 SOK-READ-SEC           SECTION.
     MOVE    ZAI-F01         TO   SOK-F01.
     READ    ZSOKMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF   INVALID-FLG  =  "1"
          MOVE  ALL NC"＊"  TO  SOKNM
     END-IF.
* ＮＯＴ ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG   =   ZERO
          MOVE  SOK-F02  TO   SOKNM
     END-IF.
 SOK-READ-END.
     EXIT.
************************************************************
*      2.3       商品名の取得                              *
************************************************************
 SHO-READ-SEC           SECTION.
     MOVE    ZAI-F021        TO   MEI-F01.
     MOVE    ZAI-F022        TO   MEI-F012.
     READ    HMEIMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF   INVALID-FLG  =  "1"
          MOVE  ALL  NC"＊"   TO  SHONM
     END-IF.
* ＮＯＴ ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG   =   ZERO
          MOVE  MEI-F021 TO   WSHONM1
          MOVE  MEI-F022 TO   WSHONM2
          MOVE  WSHONM   TO   SHONM
     END-IF.
 SHO-READ-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   PRINTF   ZZAIMS  ZSOKMS  HMEIMS
              HJYOKEN.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
