****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　商品コード変換テーブルリスト      *
*    作成日／更新日　　　：　93/05/27                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品コード変換テーブルリストの　　*
*                            出力を行う．                      *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMT0080B.
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
     SELECT   DSPF      ASSIGN  TO   GS-DSPF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        SYMBOLIC DESTINATION IS  "DSP"
                        PROCESSING MODE      IS   DSP-PROC
                        GROUP                IS   DSP-GROUP
                        FORMAT               IS   DSP-FORMAT
                        SELECTED FUNCTION    IS   DSP-FUNC
                        FILE STATUS          IS   DSP-STATUS.
****<< 商品コード変換テーブル >>****************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL5
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SHO-F04
                                                 SHO-F08
                                                 SHO-F03
                        FILE      STATUS    IS   SHO-STATUS.
****<< 倉庫マスタファイル >>********************************
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
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
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     ZMT0080             OF   XMDLIB.
****<< 商品コード変換テーブル >>****************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
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
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(2).
     02 DSP-GROUP            PIC  X(8).
     02 DSP-FORMAT           PIC  X(8).
     02 DSP-STATUS           PIC  X(2).
     02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 SHO-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
 01  INVALID-FLG             PIC  9(01)  VALUE ZERO.
****  ワークエリア            ****
 01  WRK-AREA.
     02  WRK-KEY1.
         03  WRK-KEY11       PIC  X(02).
         03  WRK-KEY12       PIC  X(01).
         03  WRK-KEY13       PIC  X(03).
         03  WRK-KEY14       PIC  X(02).
     02  WRK-KEY2.
         03  WRK-KEY21       PIC  X(02).
         03  WRK-KEY22       PIC  X(01).
         03  WRK-KEY23       PIC  X(03).
         03  WRK-KEY24       PIC  X(02).
     02  WRK-KEY3.
         03  WRK-KEY31       PIC  X(02).
         03  WRK-KEY32       PIC  X(01).
         03  WRK-KEY33       PIC  X(03).
         03  WRK-KEY34       PIC  X(02).
     02  WSHONM.
         03  WSHONM1         PIC  N(15).
         03  WSHONM2         PIC  N(15).
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
     02  FILLER              PIC  X(32)  VALUE  SPACE.
     02  FILLER              PIC  N(23)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　商品変換テーブルリスト（_順）　＊＊＊".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
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
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"倉庫コード".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  MID-01              PIC  X(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  MID-02              PIC  N(05).
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"_　番".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"商品".
     02  FILLER              PIC  X(04)  VALUE  "ｺｰﾄﾞ".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"品　単".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"商品名称".
     02  FILLER              PIC  X(38)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"量販店商品ＣＤ".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"原単価".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"売単価".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"取引先".
     02  FILLER              PIC  X(04)  VALUE  "ｺｰﾄﾞ".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  M-TANA1             PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-TANA2             PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-TANA3             PIC  X(02).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  M-SHOCD             PIC  X(08).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-HINTAN            PIC  X(08).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-SHONM             PIC  N(30).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-RYOCD             PIC  X(13).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-GENTAN            PIC  Z,ZZZ,ZZZ.ZZ.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-URITAN            PIC  Z,ZZZ,ZZZ.ZZ.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-TORICD            PIC  9(08).
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  MSG-ERR3            PIC  N(30)  VALUE
            NC"倉庫コードエラー".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMT0080B".
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
                   PROCEDURE  HSHOTBL.
     MOVE   "HSHOTBL "        TO    ERR-FL-ID.
     MOVE    SHO-STATUS       TO    ERR-STCD.
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
 END     DECLARATIVES.
************************************************************
 ZMT0080B-START          SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     IF   PG-END   NOT =  "END"
          MOVE     SPACE   TO   END-FLG
          GO  TO   ZMT0080B-START
     END-IF.
     STOP     RUN.
 ZMT0080B-END.
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
     OPEN     INPUT     HSHOTBL   ZSOKMS    HMEIMS.
     OPEN     OUTPUT    PRINTF.
     MOVE    "ZMT0080  "  TO     DSP-FORMAT.
     MOVE     SPACE     TO     ZMT0080.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     MOVE     60        TO     L-CNT.
     MOVE     1         TO     P-CNT.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     SOKCD     TO     SHO-F04.
     MOVE     STANA1    TO     SHO-F08(1:1).
     MOVE     STANA2    TO     SHO-F08(2:3).
     MOVE     STANA3    TO     SHO-F08(5:2).
     MOVE     SPACE     TO     SHO-F03.
     START    HSHOTBL   KEY  IS  >=  SHO-F04  SHO-F08  SHO-F03
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
     READ     HSHOTBL
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     MOVE     SHO-F04        TO     WRK-KEY31.
     MOVE     SHO-F08(1:1)   TO     WRK-KEY32.
     MOVE     SHO-F08(2:3)   TO     WRK-KEY33.
     MOVE     SHO-F08(5:2)   TO     WRK-KEY34.
     IF       WRK-KEY3  >    WRK-KEY2
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
     WRITE    ZMT0080.
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
                IF   SOKCD  NOT  =    SPACE
                     PERFORM    SOK-READ-SEC
                     IF     MSG1      NOT  =  SPACE
                            GO  TO  DSP-SEC
                     END-IF
                ELSE
                     MOVE   MSG-ERR3  TO  MSG1
                     GO  TO  DSP-SEC
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
                MOVE     SOKCD     TO     WRK-KEY11
                MOVE     STANA1    TO     WRK-KEY12
                MOVE     STANA2    TO     WRK-KEY13
                MOVE     STANA3    TO     WRK-KEY14
                MOVE     SOKCD     TO     WRK-KEY21
                MOVE     ETANA1    TO     WRK-KEY22
                MOVE     ETANA2    TO     WRK-KEY23
                MOVE     ETANA3    TO     WRK-KEY24
                IF   WRK-KEY1 >   WRK-KEY2
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
     IF       L-CNT         >=      60
              PERFORM       MIDASI-SEC
     END-IF.
     MOVE     SHO-F08(1:1)  TO      M-TANA1.
     MOVE     SHO-F08(2:3)  TO      M-TANA2.
     MOVE     SHO-F08(5:2)  TO      M-TANA3.
     MOVE     SHO-F031      TO      M-SHOCD.
     MOVE     SHO-F032      TO      M-HINTAN.
     PERFORM     SHO-READ-SEC.
     MOVE     SHO-F01       TO      M-TORICD.
     MOVE     SHO-F02       TO      M-RYOCD.
     MOVE     SHO-F05       TO      M-GENTAN.
     MOVE     SHO-F06       TO      M-URITAN.
     IF       L-CNT    =    6
              WRITE    P-REC   FROM    MEISAI-1    AFTER  2
              ADD      2             TO      L-CNT
     ELSE
              WRITE    P-REC   FROM    MEISAI-1    AFTER  2
              ADD      2             TO      L-CNT
     END-IF.
*
     READ     HSHOTBL
              AT END   MOVE  "END"  TO  END-FLG
     END-READ.
     MOVE     SHO-F04       TO     WRK-KEY31.
     MOVE     SHO-F08(1:1)  TO     WRK-KEY32.
     MOVE     SHO-F08(2:3)  TO     WRK-KEY33.
     MOVE     SHO-F08(5:2)  TO     WRK-KEY34.
     IF       WRK-KEY3  >    WRK-KEY2
              MOVE  "END"    TO   END-FLG
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
     MOVE     SOKCD      TO      MID-01.
     PERFORM     SOK-READ-SEC.
     IF       P-CNT  NOT  = 1
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
     END-IF.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-3   AFTER  2.
     ADD      1  TO      P-CNT.
     MOVE     6  TO      L-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*      2.2       倉庫名の取得                              *
************************************************************
 SOK-READ-SEC           SECTION.
     MOVE    SOKCD           TO   SOK-F01.
     READ    ZSOKMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF   INVALID-FLG  =  "1"
          MOVE  MSG-ERR3    TO   MSG1
     END-IF.
* ＮＯＴ ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG   =   ZERO
          MOVE  SOK-F02     TO   SOKNM
          MOVE  SOK-F02     TO   MID-02
     END-IF.
 SOK-READ-END.
     EXIT.
************************************************************
*      2.3       商品名の取得                              *
************************************************************
 SHO-READ-SEC           SECTION.
     MOVE    SHO-F031        TO   MEI-F01.
     MOVE    SHO-F032        TO   MEI-F012.
     READ    HMEIMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF   INVALID-FLG  =  "1"
          MOVE  ALL  NC"＊"   TO  M-SHONM
     END-IF.
* ＮＯＴ ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG   =   ZERO
          MOVE  MEI-F021 TO   WSHONM1
          MOVE  MEI-F022 TO   WSHONM2
          MOVE  WSHONM   TO   M-SHONM
     END-IF.
 SHO-READ-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   PRINTF   HSHOTBL ZSOKMS  HMEIMS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
