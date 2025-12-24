****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　日次更新処理　　　　　　　　　    *
*    業務名　　　　　　　：　たねまる連携ＤＴ確認              *
*    モジュール名　　　　：　たねまる連携ＤＴ抽出　　　　　　　*
*    作成日／更新日　　　：　2024/02/17                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　抽出日付範囲を受取、たねまる連携  *
*                            ＤＴをワークＦに抽出する。　　    *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SUK0020B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2024/02/17.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受注売上調整計上データ
     SELECT   URIKEJF   ASSIGN    TO        DA-01-VI-URIKEJL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KEJ-F48   KEJ-F49
                                            KEJ-F01   KEJ-F02
                                            KEJ-F04   KEJ-F03
                                            KEJ-F05   KEJ-F06
                                            KEJ-F07
                        FILE  STATUS   IS   KEJ-ST.
*日次計上連携データ
     SELECT   KEIURIDT  ASSIGN    TO        DA-01-VS-KEIURIDT
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   KEI-ST.
*
*ＳＵＢ商品名称マスタ
     SELECT   SUBMEIL1  ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SBM-F011
                                            SBM-F0121
                                            SBM-F0122
                                            SBM-F0123
                        FILE  STATUS   IS   SBM-ST.
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    受注売上調整計上データ　　                               *
****************************************************************
 FD  URIKEJF
                       LABEL     RECORD    IS   STANDARD.
     COPY     URIKEJF   OF        XFDLIB
              JOINING   KEJ  AS   PREFIX.
*
******************************************************************
*    日次計上連携データ
******************************************************************
 FD  KEIURIDT
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIURIDT  OF        XFDLIB
              JOINING   KEI  AS   PREFIX.
*
******************************************************************
*    ＳＵＢ商品名称マスタ
******************************************************************
 FD  SUBMEIL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SUBMEIL1  OF        XFDLIB
              JOINING   SBM  AS   PREFIX.
*
*
*************C**************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(03)     VALUE   SPACE.
 01  SUBMEIL1-INV-FLG          PIC  X(03)     VALUE   SPACE.
 01  READ-CNT                  PIC  9(07)     VALUE   ZERO.
 01  OUTPUT-CNT                PIC  9(07)     VALUE   ZERO.
*ステータス領域
 01  STATUS-AREA.
     03  KEJ-ST                PIC  X(02).
     03  KEI-ST                PIC  X(02).
     03  SBM-ST                PIC  X(02).
 01  FILE-ERR.
     03  KEJ-ERR           PIC N(15) VALUE
         NC"受注売上調整計上ＤＴエラー".
     03  KEI-ERR           PIC N(15) VALUE
         NC"日次計上連携データエラー".
     03  SBM-ERR           PIC N(15) VALUE
         NC"ＳＵＢ商品名称マスタエラー".
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
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMNSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
*   日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE              SECTION.
 01   LINK-IN-DATES       PIC  9(08).
 01   LINK-IN-DATEE       PIC  9(08).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION  USING  LINK-IN-DATES
                                        LINK-IN-DATEE.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE URIKEJF.
     MOVE        KEJ-ST    TO        E-ST.
     MOVE        "URIKEJL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEJ-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC2              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KEIURIDT.
     MOVE        KEI-ST     TO       E-ST.
     MOVE        "KEIURIDT" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC3              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SUBMEIL1.
     MOVE        SBM-ST     TO       E-ST.
     MOVE        "SUBMEIL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SBM-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC
               UNTIL  END-FLG = "END".
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*ファイルのＯＰＥＮ
     OPEN      I-O    URIKEJF.
     OPEN      INPUT  SUBMEIL1.
     OPEN      OUTPUT KEIURIDT.
*受注売上調整計上データスタート
     MOVE     SPACE             TO    KEJ-REC.
     INITIALIZE                       KEJ-REC.
     MOVE     LINK-IN-DATES     TO    KEJ-F48.
     START  URIKEJF KEY IS  >=  KEJ-F48  KEJ-F49  KEJ-F01
                                KEJ-F02  KEJ-F04  KEJ-F03
                                KEJ-F05  KEJ-F06  KEJ-F07
            INVALID
            MOVE  "END"         TO    END-FLG
            DISPLAY NC"＃＃対象データ無し（ＳＴ）＃＃" UPON CONS
            GO                  TO    INIT-EXIT
     END-START.
*
     PERFORM  URIKEJF-READ-SEC.
     IF   END-FLG =  "END"
          DISPLAY NC"＃＃対象データ無し（ＲＤ）＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    受注売上調整計上データ
****************************************************************
 URIKEJF-READ-SEC     SECTION.
     MOVE     "URIKEJF-READ-SEC"     TO   S-NAME.
*
     READ  URIKEJF   NEXT  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   URIKEJF-READ-EXIT
     END-READ.
*
     ADD   1                 TO   READ-CNT
     IF   READ-CNT(5:3)  =  "000" OR "500"
          DISPLAY "## READ-CNT = " READ-CNT " #" UPON CONS
     END-IF.
*対象範囲チェック
     IF   LINK-IN-DATES  <=  KEJ-F48
     AND  LINK-IN-DATEE  >=  KEJ-F48
          CONTINUE
     ELSE
           MOVE   "END"      TO   END-FLG
     END-IF.
*
 URIKEJF-READ-EXIT.
     EXIT.
****************************************************************
*    ＳＵＢ商品名称マスタ読込
****************************************************************
 SUBMEIL1-READ-SEC     SECTION.
     MOVE     "SUBMEIL1-READ-SEC"     TO   S-NAME.
*
     READ  SUBMEIL1
           INVALID     MOVE  "INV"    TO   SUBMEIL1-INV-FLG
           NOT INVALID MOVE  SPACE    TO   SUBMEIL1-INV-FLG
     END-READ.
*
 SUBMEIL1-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     MOVE      SPACE         TO   KEI-REC.
     INITIALIZE                   KEI-REC.
*
     MOVE      KEJ-F01       TO   KEI-F01.
     MOVE      KEJ-F02       TO   KEI-F02.
     MOVE      KEJ-F03       TO   KEI-F03.
     MOVE      KEJ-F04       TO   KEI-F04.
     MOVE      KEJ-F05       TO   KEI-F05.
     MOVE      KEJ-F06       TO   KEI-F06.
     MOVE      KEJ-F07       TO   KEI-F07.
     MOVE      KEJ-F08       TO   KEI-F08.
     MOVE      KEJ-F09       TO   KEI-F09.
     MOVE      KEJ-F10       TO   KEI-F10.
     MOVE      KEJ-F11       TO   KEI-F11.
     MOVE      KEJ-F12       TO   KEI-F12.
     MOVE      KEJ-F13       TO   KEI-F13.
     MOVE      KEJ-F14       TO   KEI-F14.
     MOVE      KEJ-F15       TO   KEI-F15.
     MOVE      KEJ-F16       TO   KEI-F16.
     MOVE      KEJ-F17       TO   KEI-F17.
     MOVE      KEJ-F18       TO   KEI-F18.
     MOVE      KEJ-F21       TO   KEI-F19.
     MOVE      KEJ-F22       TO   KEI-F20.
     MOVE      KEJ-F23       TO   KEI-F21.
     MOVE      KEJ-F24       TO   KEI-F22.
     MOVE      KEJ-F25       TO   KEI-F23.
     MOVE      KEJ-F281      TO   KEI-F24.
     MOVE      KEJ-F282      TO   KEI-F25.
     MOVE      KEJ-F35       TO   KEI-F26.
     MOVE      KEJ-F48       TO   KEI-F27.
     MOVE      KEJ-F49       TO   KEI-F28.
*ＳＵ商品名称マスタ存在チェック
     MOVE      KEJ-F14       TO   SBM-F011.
     MOVE      KEJ-F15(1:5)  TO   SBM-F0121.
     MOVE      KEJ-F15(6:2)  TO   SBM-F0122.
     MOVE      KEJ-F15(8:1)  TO   SBM-F0123.
     PERFORM   SUBMEIL1-READ-SEC.
     IF  SUBMEIL1-INV-FLG  =  "INV"
         MOVE  ALL "*"       TO   KEI-F29
     ELSE
         MOVE  SBM-D01       TO   KEI-F29
     END-IF.
     MOVE      KEJ-F06(1:6)  TO   KEI-F30.
*
     WRITE  KEI-REC.
     ADD       1             TO   OUTPUT-CNT.
*
     PERFORM  URIKEJF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"      TO   S-NAME.
*
*ファイルのＯＰＥＮ
     CLOSE     URIKEJF  KEIURIDT  SUBMEIL1.
*
     DISPLAY "## READ-CNT   = " READ-CNT   " ##"  UPON CONS.
     DISPLAY "## OUTPUT-CNT = " OUTPUT-CNT " ##"  UPON CONS.
*
 END-EXIT.
     EXIT.
