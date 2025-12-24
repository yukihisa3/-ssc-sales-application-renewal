*************************************************************
*    顧客名　　　　　：　（株）サカタのタネ殿               *
*    業務名　　　　　：　共通システム                       *
*    モジュール名　　：　プログラム実行結果（対象データなし）
*    作成日／更新日　：　2022/10/12                         *
*    作成者／更新者　：　NAV                                *
*    処理概要説明　　：　　　　　　　　　　　　　　　　　　 *
*                        　　　　　　　　　　　　　　       *
*    使用ＭＥＤ      ：  FMG00301                           *
*************************************************************
 IDENTIFICATION                  DIVISION.
 PROGRAM-ID.                     SMG0032I.
 ENVIRONMENT                     DIVISION.
 CONFIGURATION                   SECTION.
 SOURCE-COMPUTER.                GP6000.
 OBJECT-COMPUTER.                GP6000.
 SPECIAL-NAMES.
         SYSOUT        IS        SOUT
         STATION       IS        STA
         CONSOLE       IS        CSL.
*
 INPUT-OUTPUT                    SECTION.
 FILE-CONTROL.
*－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－*
*            画面ファイル　　　　　　　　　　　　　　　　　　  *
*－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－*
     SELECT    DSPF       ASSIGN       TO   GS-DSPF
               SYMBOLLIC  DESTINATION  IS   "DSP"
               FORMAT                  IS   DSP-FORMAT
               GROUP                   IS   DSP-GROUP
               PROCESSING              IS   DSP-PROC
               FUNCTION                IS   DSP-FUNC
               CONTROL                 IS   DSP-CONT
               STATUS                  IS   DSP-ST.
****************************************************************
 DATA                            DIVISION.
****************************************************************
 FILE                            SECTION.
*---< 表示ファイル >-------*
 FD  DSPF              LABEL     RECORD    IS   STANDARD.
     COPY    FMG00301  OF        XMDLIB.
****************************************************************
 WORKING-STORAGE                 SECTION.
****************************************************************
 01  PGM-ID                  PIC  X(08)    VALUE     "SMG0032I".
*
 01  FLAGS.
     03  END-FLG             PIC  X(03)    VALUE  SPACE.
*
*画面制御用
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-FUNC            PIC  X(04).
     03  DSP-CONT            PIC  X(06).
     03  DSP-ST              PIC  X(02)    VALUE  SPACE.
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
 LINKAGE                         SECTION.
 01  LINK-OK                 PIC  X(01).
 01  LINK-MSG1               PIC  N(20).
 01  LINK-MSG2               PIC  N(20).
 01  LINK-MSG3               PIC  N(20).
 01  LINK-MSG4               PIC  N(20).
 01  LINK-MSG5               PIC  N(20).
****************************************************************
 PROCEDURE                   DIVISION      USING  LINK-OK
                                                  LINK-MSG1
                                                  LINK-MSG2
                                                  LINK-MSG3
                                                  LINK-MSG4
                                                  LINK-MSG5.
****************************************************************
 DECLARATIVES.
*---------<  画面ファイル　>-----------------------------------*
 DSPF-ERR-SECTION               SECTION.
        USE     AFTER     EXCEPTION   PROCEDURE  DSPF.
        DISPLAY "### " PGM-ID " DSPF        ERROR " DSP-ST " "
             HEN-DATE " " HEN-TIME " ###" UPON CSL.
        STOP      RUN.
 END    DECLARATIVES.
****************************************************************
*    < 0.0 >           基本モジュール
****************************************************************
 PROGRAM-START-SEC           SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC   UNTIL   END-FLG  =  "END".
     PERFORM      FIN-SEC.
     STOP         RUN.
 PROGRAM-END-EXIT.
     EXIT.
****************************************************************
*    < 1.0 >           初期処理
****************************************************************
 INIT-SEC                    SECTION.
* ファイルオープン
     OPEN    I-O    DSPF.
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
*
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
     MOVE      HEN-TIME           TO   STIME.
     MOVE      HEN-DATE           TO   SDATE.
 INIT-EXT.
     EXIT.
****************************************************************
*    < 2.0 >           主　処　理
****************************************************************
 MAIN-SEC                   SECTION.
*初期画面表示
     PERFORM   DSP-INIT-SEC.
 MAIN-010.
     PERFORM   DSP-READ-SEC
     EVALUATE  DSP-FUNC
         WHEN  "F005"
               MOVE "END"          TO   END-FLG
               MOVE  ZERO          TO   PROGRAM-STATUS
         WHEN  "E000"
               IF  LSTOUT = "Y" OR "N"
                   MOVE "END"          TO   END-FLG
                   MOVE  ZERO          TO   PROGRAM-STATUS
                   MOVE  SPACE         TO   ERRMSG
                   IF LSTOUT = "Y"
                      CALL "SMG0020L" USING LINK-MSG1 LINK-MSG2
                                            LINK-MSG3 LINK-MSG4
                                            LINK-MSG5
                   END-IF
               ELSE
                   MOVE NC"もう一度、入力して下さい。" TO ERRMSG
                   PERFORM   DSP-WRITE-SEC
                   GO                  TO   MAIN-010
               END-IF
     END-EVALUATE.
     PERFORM   DSP-WRITE-SEC.
 MAIN-EXIT.
     EXIT.
****************************************************************
*    < 3.0 >           終了処理
****************************************************************
 FIN-SEC                     SECTION.
     CLOSE          DSPF.
 FIN-EXIT.
     EXIT.
****************************************************************
*    < ALL >      初期画面表示
****************************************************************
 DSP-INIT-SEC                SECTION.
     MOVE    LOW-VALUE           TO   FMG00301.
     MOVE   "FMG00301"           TO   DSP-FORMAT.
*    MOVE   "CL"                 TO   DSP-PROC.
     MOVE    PGM-ID              TO   PGID.
     MOVE    HEN-TIME            TO   STIME.
     MOVE    HEN-DATE            TO   SDATE.
     MOVE    LINK-MSG1           TO   PGNM.
     MOVE    LINK-MSG2           TO   KEKA1.
*----MOVE NC"　　　　　　処理対象データ　　　　　　"
*----                            TO   KEKA1.
     MOVE    LINK-MSG3           TO   KEKA2.
*----MOVE NC"　　　　　　　　無！！　　　　　　　　"
*----                            TO   KEKA2.
     MOVE    LINK-MSG4           TO   KEKA3.
*----MOVE NC"　　　　　　　　無！！　　　　　　　　"
*----                            TO   KEKA3.
     MOVE    LINK-MSG5           TO   KEKA4.
*----MOVE NC"　　　　　　確認してください　　　　　"
*----                            TO   KEKA4.
     MOVE    "N"                 TO   LSTOUT.
*----MOVE    " "                 TO   LSTOUT.
     IF      LINK-OK  =  "1"
             MOVE  NC"『　正　常　終　了　』"  TO  PGKEKA
             MOVE  "V"           TO   EDIT-OPTION OF PGKEKA
             MOVE  "B"           TO   EDIT-COLOR  OF PGKEKA
     ELSE
             MOVE  NC"『　異　常　終　了　』"  TO  PGKEKA
             MOVE  "V"           TO   EDIT-OPTION OF PGKEKA
             MOVE  "R"           TO   EDIT-COLOR  OF PGKEKA
             MOVE  "V"           TO   EDIT-OPTION OF PGNM
             MOVE  "V"           TO   EDIT-OPTION OF KEKA1
             MOVE  "V"           TO   EDIT-OPTION OF KEKA2
             MOVE  "V"           TO   EDIT-OPTION OF KEKA3
             MOVE  "V"           TO   EDIT-OPTION OF KEKA4
             MOVE  "R"           TO   EDIT-COLOR  OF PGNM
             MOVE  "R"           TO   EDIT-COLOR  OF KEKA1
             MOVE  "R"           TO   EDIT-COLOR  OF KEKA2
             MOVE  "R"           TO   EDIT-COLOR  OF KEKA3
             MOVE  "R"           TO   EDIT-COLOR  OF KEKA4
     END-IF.
     PERFORM DSP-WRITE-SEC.
 DSP-INIT-EXT.
     EXIT.
****************************************************************
*    < ALL >      画面表示処理
****************************************************************
 DSP-WRITE-SEC              SECTION.
     MOVE     SPACE         TO    DSP-PROC.
     MOVE    "SCREEN"       TO    DSP-GROUP.
     WRITE    FMG00301.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*    < ALL >      画面入力処理
****************************************************************
 DSP-READ-SEC               SECTION.
     MOVE    "LSTOUT"       TO   DSP-GROUP.
     MOVE    "NE"           TO   DSP-PROC.
     READ    DSPF       AT  END
                            GO   TO    DSP-READ-EXIT
     END-READ.
     IF    DSP-ST    NOT =  ZERO
           GO  TO    DSP-READ-SEC
     END-IF.
 DSP-READ-EXIT.
     EXIT.
