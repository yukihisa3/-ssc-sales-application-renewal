****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＣＳＶ出力　　　　　　　　　　　　*
*    モジュール名　　　　：　商品名称マスタＣＳＶ出力　        *
*    作成日／更新日　　　：　06/01/11                          *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品名称マスタＣＳＶ出力を行う。  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SVV0020B.
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
****<< 商品名称マスタ　ＣＳＶ >>******************************
     SELECT   SVVMEI    ASSIGN  TO   DA-01-S-SVVMEI
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****<< 商品名称マスタ　　   >>******************************
     SELECT   HMEIMS    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE STATUS          IS  MEI-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FVV00201           OF   XMDLIB.
****<< 商品名称マスタ　ＣＳＶ >>******************************
 FD    SVVMEI.
       COPY     SVVMEI    OF        XFDLIB
                JOINING   CSV       PREFIX.
****<< 商品名称マスタ　　　　 >>******************************
 FD    HMEIMS.
       COPY     HMEIMS    OF        XFDLIB
                JOINING   MEI       PREFIX.
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
     02 CSV-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
****  ワークエリア　          ****
 01  WK-AREA.
     02 WK-STAN.
         03  WK-STAN1        PIC  X(05)  VALUE  SPACE.
         03  WK-STAN2        PIC  X(02)  VALUE  SPACE.
         03  WK-STAN3        PIC  X(01)  VALUE  SPACE.
     02 WK-ETAN.
         03  WK-ETAN1        PIC  X(05)  VALUE  SPACE.
         03  WK-ETAN2        PIC  X(02)  VALUE  SPACE.
         03  WK-ETAN3        PIC  X(01)  VALUE  SPACE.
****  カウンタ                ****
 01  HMEIMS-CNT              PIC  9(8)   VALUE  ZERO.
 01  SVVMEI-CNT              PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SVV0020B".
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
                   PROCEDURE  SVVMEI.
     MOVE   "SVVMEI  "        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
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
 END     DECLARATIVES.
************************************************************
 SVV0020B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SVV0020B-END.
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
     OPEN     INPUT     HMEIMS.
     OPEN     OUTPUT    SVVMEI.
     MOVE    "FVV00201"  TO     DSP-FORMAT.
     MOVE     SPACE     TO     FVV00201.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     SSYOCD    TO     MEI-F011.
     MOVE     STAN1     TO     MEI-F0121.
     MOVE     STAN2     TO     MEI-F0122.
     MOVE     STAN3     TO     MEI-F0123.
     START    HMEIMS    KEY  IS  >=  MEI-F011  MEI-F0121
                                     MEI-F0122 MEI-F0123
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
 010-INIT.
     READ     HMEIMS
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     IF       MEI-F011         >    ESYOCD
              MOVE  "END"    TO   END-FLG
              GO TO  INIT-END
     ELSE
              IF     MEI-F012     >     WK-ETAN
              OR     MEI-F012     <     WK-STAN
                     GO TO  010-INIT
              END-IF
     END-IF.
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
     WRITE    FVV00201.
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
                MOVE   "M"       TO  EDIT-OPTION  OF  SSYOCD
                MOVE   "M"       TO  EDIT-OPTION  OF  ESYOCD
                MOVE   "M"       TO  EDIT-OPTION  OF  STAN1
                MOVE   "M"       TO  EDIT-OPTION  OF  STAN2
                MOVE   "M"       TO  EDIT-OPTION  OF  STAN3
                MOVE   "M"       TO  EDIT-OPTION  OF  ETAN1
                MOVE   "M"       TO  EDIT-OPTION  OF  ETAN2
                MOVE   "M"       TO  EDIT-OPTION  OF  ETAN3
                MOVE   " "       TO  EDIT-CURSOR  OF  SSYOCD
                MOVE   " "       TO  EDIT-CURSOR  OF  ESYOCD
                MOVE   " "       TO  EDIT-CURSOR  OF  STAN1
                MOVE   " "       TO  EDIT-CURSOR  OF  STAN2
                MOVE   " "       TO  EDIT-CURSOR  OF  STAN3
                MOVE   " "       TO  EDIT-CURSOR  OF  ETAN1
                MOVE   " "       TO  EDIT-CURSOR  OF  ETAN2
                MOVE   " "       TO  EDIT-CURSOR  OF  ETAN3
                MOVE   SPACE     TO  MSG1
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
                IF   SSYOCD =    SPACE
                     MOVE  "        " TO  SSYOCD
                END-IF
                IF   ESYOCD =    SPACE
                     MOVE  "99999999" TO  ESYOCD
                END-IF
                IF   STAN1  =    SPACE
                AND  STAN2  =    SPACE
                AND  STAN3  =    SPACE
                     MOVE  "     "    TO  STAN1
                     MOVE  "  "       TO  STAN2
                     MOVE  " "        TO  STAN3
                END-IF
                IF   ETAN1  =    SPACE
                AND  ETAN2  =    SPACE
                AND  ETAN3  =    SPACE
                     MOVE  "99999"    TO  ETAN1
                     MOVE  "99"       TO  ETAN2
                     MOVE  "9"        TO  ETAN3
                END-IF
                MOVE        STAN1     TO  WK-STAN1
                MOVE        STAN2     TO  WK-STAN2
                MOVE        STAN3     TO  WK-STAN3
                MOVE        ETAN1     TO  WK-ETAN1
                MOVE        ETAN2     TO  WK-ETAN2
                MOVE        ETAN3     TO  WK-ETAN3
                IF   SSYOCD >   ESYOCD
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  SSYOCD
                     MOVE   "R"       TO  EDIT-OPTION  OF  ESYOCD
                     MOVE   "C"       TO  EDIT-CURSOR  OF  SSYOCD
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  SSYOCD
                     MOVE   "M"       TO  EDIT-OPTION  OF  ESYOCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  SSYOCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  ESYOCD
                END-IF
                IF   WK-STAN >   WK-ETAN
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  STAN1
                     MOVE   "R"       TO  EDIT-OPTION  OF  STAN2
                     MOVE   "R"       TO  EDIT-OPTION  OF  STAN3
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETAN1
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETAN2
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETAN3
                     MOVE   "C"       TO  EDIT-CURSOR  OF  STAN1
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  STAN1
                     MOVE   "M"       TO  EDIT-OPTION  OF  STAN2
                     MOVE   "M"       TO  EDIT-OPTION  OF  STAN3
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETAN1
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETAN2
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETAN3
                     MOVE   " "       TO  EDIT-CURSOR  OF  STAN1
                     MOVE   " "       TO  EDIT-CURSOR  OF  STAN2
                     MOVE   " "       TO  EDIT-CURSOR  OF  STAN3
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETAN1
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETAN2
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETAN3
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
*商品名称マスタカウントアップ
     ADD        1           TO      HMEIMS-CNT.
     PERFORM  SVVMEI-WRITE-SEC.
 010-MEI.
     READ     HMEIMS
              AT END   MOVE  "END"  TO  END-FLG
              GO TO MAIN-END
     END-READ.
     IF       MEI-F011      >       ESYOCD
              MOVE  "END"  TO  END-FLG
              GO TO MAIN-END
     END-IF.
     IF       MEI-F012      >       WK-ETAN
     OR       MEI-F012      <       WK-STAN
              GO       TO     010-MEI
     END-IF.
 MAIN-END.
     EXIT.
************************************************************
*      _１      商品名称マスタ　ＣＳＶ出力                *
************************************************************
 SVVMEI-WRITE-SEC       SECTION.
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-A01  CSV-A02  CSV-A03
                                    CSV-A04  CSV-A05  CSV-A06
                                    CSV-A07  CSV-A08  CSV-A09
                                    CSV-A10  CSV-A11.
*カウントアップ
     ADD       1            TO      SVVMEI-CNT.
*商品コード
     MOVE      MEI-F011     TO      CSV-F01.
*品単コード
     MOVE      MEI-F012     TO      CSV-F02.
*商品名（日本語）_
     MOVE      MEI-F021     TO      CSV-F032.
*商品名（日本語）_
     MOVE      MEI-F022     TO      CSV-F042.
*商品名（カナ）_
     MOVE      MEI-F031     TO      CSV-F05.
*商品名（カナ）_
     MOVE      MEI-F032     TO      CSV-F06.
*原価単価
     MOVE      MEI-F041     TO      CSV-F07.
*原単価
     MOVE      MEI-F042     TO      CSV-F08.
*売単価　　　
     MOVE      MEI-F043     TO      CSV-F09.
*仕入先コード
     MOVE      MEI-F05      TO      CSV-F10.
*ＪＡＮコード
     MOVE      MEI-F06      TO      CSV-F11.
*入数
     MOVE      MEI-F07      TO      CSV-F12.
*制御コード
     MOVE      X"28"        TO      CSV-F031  CSV-F041.
     MOVE      X"29"        TO      CSV-F033  CSV-F043.
*レコード出力
     WRITE     CSV-REC.

 SVVMEI-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   SVVMEI  HMEIMS.

     IF    HMEIMS-CNT NOT = ZERO
     DISPLAY  " HMEIMS-READ-CNT = " HMEIMS-CNT UPON CONS
     DISPLAY  " SVVMEI-WRITE-CNT = " SVVMEI-CNT UPON CONS
     END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
