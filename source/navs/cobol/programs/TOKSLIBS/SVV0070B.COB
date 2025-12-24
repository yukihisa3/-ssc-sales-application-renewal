****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＣＳＶ出力　　　　　　　　　　　　*
*    モジュール名　　　　：　ルート条件マスタＣＳＶ出力        *
*    作成日／更新日　　　：　06/01/10                          *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　ルート条件マスタのＣＳＶ出力を　  *
*                            行う。　                          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SVV0070B.
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
****<< ルート条件マスタＣＳＶ >>******************************
     SELECT   SVVRUT    ASSIGN  TO   DA-01-S-SVVRUT
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****<<  ルート条件マスタ    >>******************************
     SELECT   JHMRUTF   ASSIGN  TO   DA-01-VI-JHMRUTL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  RUT-F01 RUT-F02
                                                 RUT-F03
                        FILE STATUS          IS  RUT-STATUS.
****<<  取引先マスタ　　    >>******************************
     SELECT   HTOKMS    ASSIGN  TO   DA-01-VI-TOKMS2
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  TOK-F01
                        FILE STATUS          IS  TOK-STATUS.
****<<  倉庫マスタ　　　    >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  SOK-F01
                        FILE STATUS          IS  SOK-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FVV00701           OF   XMDLIB.
****<< ルート条件マスタＣＳＶ >>******************************
 FD    SVVRUT.
       COPY     SVVRUT    OF        XFDLIB
                JOINING   CSV       PREFIX.
****<< ルート条件マスタ　　　 >>******************************
 FD    JHMRUTF.
       COPY     JHMRUTF   OF        XFDLIB
                JOINING   RUT       PREFIX.
****<< 取引先マスタ　　　　　 >>******************************
 FD    HTOKMS.
       COPY     HTOKMS    OF        XFDLIB
                JOINING   TOK       PREFIX.
****<< 倉庫マスタ　　　　　　 >>******************************
 FD    ZSOKMS.
       COPY     ZSOKMS    OF        XFDLIB
                JOINING   SOK       PREFIX.
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
     02 RUT-STATUS           PIC  X(2).
     02 TOK-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
****  カウンタ                ****
 01  JHMRUTF-CNT             PIC  9(8)   VALUE  ZERO.
 01  SVVRUT-CNT              PIC  9(8)   VALUE  ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SVV0070B".
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
                   PROCEDURE  SVVRUT.
     MOVE   "SVVRUT  "        TO    ERR-FL-ID.
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
                   PROCEDURE  JHMRUTF.
     MOVE   "JHMRUTF "        TO    ERR-FL-ID.
     MOVE    RUT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTOKMS.
     MOVE   "HTOKMS  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 SVV0070B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
*    IF   PG-END   NOT =  "END"
*         MOVE     SPACE   TO   END-FLG
*         GO  TO   SVV0070B-START
*    END-IF.
     STOP     RUN.
 SVV0070B-END.
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
     OPEN     INPUT     JHMRUTF  HTOKMS  ZSOKMS.
     OPEN     OUTPUT    SVVRUT.
     MOVE    "FVV00701"  TO     DSP-FORMAT.
     MOVE     SPACE     TO     FVV00701.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     STCODE    TO     RUT-F01.
     MOVE     SPACE     TO     RUT-F02  RUT-F03.
     START    JHMRUTF   KEY  IS  >=  RUT-F01  RUT-F02  RUT-F03
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
     READ     JHMRUTF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     IF       RUT-F01   >    ETCODE
              MOVE  "END"    TO   END-FLG
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
     WRITE    FVV00701.
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
                MOVE  "M"     TO   EDIT-OPTION  OF  STCODE
                MOVE  "M"     TO   EDIT-OPTION  OF  ETCODE
                MOVE  SPACE   TO   EDIT-CURSOR  OF  STCODE
                MOVE  SPACE   TO   EDIT-CURSOR  OF  ETCODE
                MOVE  SPACE   TO   MSG1
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
                IF   STCODE =    SPACE
                     MOVE   00000000  TO  STCODE
                END-IF
                IF   ETCODE =    SPACE
                     MOVE   99999999  TO  ETCODE
                END-IF
                IF   STCODE >   ETCODE
                     MOVE   MSG-ERR2  TO  MSG1
                     MOVE   "R"       TO  EDIT-OPTION OF STCODE
                     MOVE   "R"       TO  EDIT-OPTION OF ETCODE
                     MOVE   "C"       TO  EDIT-CURSOR OF STCODE
                     GO TO  DSP-SEC
                END-IF
                     MOVE  "M"     TO   EDIT-OPTION  OF  STCODE
                     MOVE  "M"     TO   EDIT-OPTION  OF  ETCODE
                     MOVE  SPACE   TO   EDIT-CURSOR  OF  STCODE
                     MOVE  SPACE   TO   EDIT-CURSOR  OF  ETCODE
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
*条件ルートマスタカウントアップ
     ADD        1           TO      JHMRUTF-CNT.
     PERFORM  SVVRUT-WRITE-SEC.

     READ     JHMRUTF
              AT END   MOVE  "END"  TO  END-FLG
     END-READ.
     IF       RUT-F01       >       ETCODE
                       MOVE  "END"  TO  END-FLG
     END-IF.
 MAIN-END.
     EXIT.
************************************************************
*      _１      ルート条件マスタＣＳＶ出力                *
************************************************************
 SVVRUT-WRITE-SEC       SECTION.
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-A01  CSV-A02  CSV-A03
                                    CSV-A04  CSV-A05  CSV-A06.
*カウントアップ
     ADD       1            TO      SVVRUT-CNT.
*取引先コード
     MOVE      RUT-F01      TO      CSV-F01.
*取引先名称
     MOVE      RUT-F01      TO      TOK-F01.
     READ      HTOKMS
        INVALID  KEY
               MOVE   SPACE     TO  CSV-F022
        NOT  INVALID
               MOVE   TOK-F02   TO  CSV-F022
     END-READ.
*分類
     MOVE      RUT-F02      TO      CSV-F03.
*ルート
     MOVE      RUT-F03      TO      CSV-F04.
*ルート取引先名称
     MOVE      RUT-F04      TO      CSV-F052.
*振分倉庫コード
     MOVE      RUT-F05      TO      CSV-F06.
*振分倉庫名称
     MOVE      RUT-F05      TO      SOK-F01.
     READ      ZSOKMS
        INVALID  KEY
               MOVE   SPACE     TO  CSV-F072
        NOT  INVALID
               MOVE   SOK-F02   TO  CSV-F072
     END-READ.
*制御コード
     MOVE      X"28"        TO      CSV-F021  CSV-F051  CSV-F071.
     MOVE      X"29"        TO      CSV-F023  CSV-F053  CSV-F073.
*レコード出力
     WRITE     CSV-REC.

 SVVRUT-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   SVVRUT  JHMRUTF  HTOKMS  ZSOKMS.

     IF   JHMRUTF-CNT  NOT = ZERO
          DISPLAY  " JHMRUT-READ-CNT = " JHMRUTF-CNT UPON CONS
          DISPLAY  " SVVRUT-WRITE-CNT = " SVVRUT-CNT UPON CONS
     END-IF.

 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
