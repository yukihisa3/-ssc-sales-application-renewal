****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　当日スケジュールマスタ更新        *
*    作成日／更新日　　　：　99/09/17                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0060B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/09/17.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*当日スケジュールマスタ
     SELECT  JHMTJSF   ASSIGN    TO        DA-01-VI-JHMTJSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 当日スケジュールマスタ                             *
****************************************************************
 FD  JHMTJSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTJSF   OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
*時間編集領域
 01  WK-TIME.
     03  WK-TIME-HHMM             PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-SS               PIC  9(04)  VALUE  ZERO.
*フラグ領域
 01  WK-FLG.
     03  JHMTJSF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TJS-ERR           PIC N(15) VALUE
         NC"当日スケジュールマスタエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD.
     03  LINK-IN-YYYY      PIC 9(04).
     03  LINK-IN-MM        PIC 9(02).
     03  LINK-IN-DD        PIC 9(02).
 01  LINK-IN-TIME          PIC 9(04).
 01  LINK-IN-TOKCD         PIC 9(08).
 01  LINK-IN-DTKEN         PIC 9(05).
 01  LINK-IN-DPKEN         PIC 9(05).
 01  LINK-IN-JFLG          PIC 9(01).
 01  LINK-IN-KFLG          PIC 9(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-IN-KBN
                                           LINK-IN-YMD
                                           LINK-IN-TIME
                                           LINK-IN-TOKCD
                                           LINK-IN-DTKEN
                                           LINK-IN-DPKEN
                                           LINK-IN-JFLG
                                           LINK-IN-KFLG.
**************************************************************
 DECLARATIVES.
 TJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMTJSF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE        "JHMTJSF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TJS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O     JHMTJSF.
*現在、時刻の取得
     ACCEPT    WK-TIME   FROM  TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*ファイルＫＥＹ項目セット
     MOVE      LINK-IN-YMD   TO   TJS-F01.
     MOVE      LINK-IN-TIME  TO   TJS-F02.
     MOVE      LINK-IN-TOKCD TO   TJS-F03.
*ファイル読込み
     READ      JHMTJSF
               INVALID
                 MOVE  "INV"   TO   JHMTJSF-INV-FLG
               NOT  INVALID
                 MOVE  SPACE   TO   JHMTJSF-INV-FLG
     END-READ.
*処理区分により各項目更新
     IF        LINK-IN-KBN  =  "1"
               IF   JHMTJSF-INV-FLG  =  "INV"
                    MOVE SPACE         TO  TJS-REC
                    INITIALIZE             TJS-REC
                    MOVE LINK-IN-YMD   TO  TJS-F01
                    MOVE LINK-IN-TIME  TO  TJS-F02
                    MOVE LINK-IN-TOKCD TO  TJS-F03
                    MOVE WK-TIME-HHMM  TO  TJS-F04
                    WRITE TJS-REC
               ELSE
                    MOVE WK-TIME-HHMM  TO  TJS-F04
                    REWRITE TJS-REC
               END-IF
               GO   TO   MAIN-EXIT
     END-IF.
*処理区分が’１’以外の場合
*処理区分が’１’以外で、マスタが読込めない場合は処理終了へ
     IF        JHMTJSF-INV-FLG  NOT =  SPACE
               GO   TO   MAIN-EXIT
     END-IF.
*
     EVALUATE  LINK-IN-KBN
         WHEN  "2"
                MOVE WK-TIME-HHMM  TO  TJS-F05
                REWRITE TJS-REC
         WHEN  "3"
                MOVE WK-TIME-HHMM  TO  TJS-F06
                REWRITE TJS-REC
         WHEN  "4"
                MOVE WK-TIME-HHMM  TO  TJS-F07
                REWRITE TJS-REC
         WHEN  "5"
                MOVE LINK-IN-DTKEN TO  TJS-F08
                MOVE LINK-IN-DPKEN TO  TJS-F09
                REWRITE TJS-REC
         WHEN  "6"
                MOVE LINK-IN-JFLG  TO  TJS-F10
                REWRITE TJS-REC
         WHEN  "7"
                MOVE LINK-IN-KFLG  TO  TJS-F11
                REWRITE TJS-REC
     END-EVALUATE.

*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JHMTJSF.
*
 END-EXIT.
     EXIT.
*****************<<  SCV0060B   END PROGRAM  >>******************
