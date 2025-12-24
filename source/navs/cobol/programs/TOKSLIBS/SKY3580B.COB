****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信                            *
*    モジュール名　　　　：　ＰＧ実行結果リスト                *
*    作成日／作成者　　　：　2000/08/21 T.TAKAHASHI            *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY3580B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/08/21.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*プリント定義ファイル
     SELECT     PRTFILE    ASSIGN    TO        GS-PRTF
                PROCESSING MODE      IS        PRT-PROC
                GROUP                IS        PRT-GROUP
                FORMAT               IS        PRT-FORMAT
                CONTROL              IS        PRT-CNTL
                FILE STATUS          IS        PRT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
     COPY        FKY35011  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  PRT-ST              PIC X(02).
*帳票制御用領域
 01  PRT-CONTROL.
     03  PRT-PROC            PIC  X(02).
     03  PRT-GROUP           PIC  X(08).
     03  PRT-FORMAT          PIC  X(08).
     03  PRT-CNTL            PIC  X(06).
*
 01  FILE-ERR.
     03  PRT-ERR             PIC N(15) VALUE
                        NC"プリンターエラー".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN                  PIC X(01).
 01  LINK-IN-YMD6                 PIC 9(06).
 01  LINK-IN-YMD8                 PIC 9(08).
 01  LINK-OUT-RET                 PIC X(01).
 01  LINK-OUT-YMD                 PIC 9(08).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION.
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 MAIN-SEC                  SECTION.
*ファイルＯＰＥＮ
     OPEN        OUTPUT      PRTFILE.
     MOVE        SPACE       TO   PRT-FKY35011.
*システム日付・時刻の取得
     ACCEPT   WK-DATE        FROM DATE.
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     WK-DATE        TO   LINK-IN-YMD6.
     MOVE     ZERO           TO   LINK-IN-YMD8.
     MOVE     ZERO           TO   LINK-OUT-RET.
     MOVE     ZERO           TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"    USING LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD   TO   DATE-AREA.
*画面表示日付編集
     MOVE     SYS-DATE(1:4)  TO   HEN-DATE-YYYY.
     MOVE     SYS-DATE(5:2)  TO   HEN-DATE-MM.
     MOVE     SYS-DATE(7:2)  TO   HEN-DATE-DD.
     MOVE     HEN-DATE       TO   PRT-SDATE.
*
*システム時刻取得
     ACCEPT    WK-TIME       FROM TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)  TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)  TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)  TO   HEN-TIME-SS.
     MOVE      HEN-TIME      TO   PRT-STIME.
*
*項目設定
***  受信結果／エラー内容
     MOVE  NC"受領有り"  TO        PRT-JKEKKA.
     MOVE  NC"ホーマック関東の受領データの受信を行いま"
                         TO        PRT-ERRNM1.
     MOVE  NC"した。受領書を発行して下さい。（植物）　"
                         TO        PRT-ERRNM2.
*プリント出力
     MOVE        SPACE       TO   PRT-CONTROL.
     MOVE        "FKY35011"  TO   PRT-FORMAT.
     MOVE        "SCREEN"    TO   PRT-GROUP.
     WRITE       PRT-FKY35011.
*終了処理
     CLOSE       PRTFILE.
*
 MAIN-SEC-EXIT.
     EXIT.
