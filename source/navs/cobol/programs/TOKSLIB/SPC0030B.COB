****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＰＣ連携                          *
*    業務名　　　　　　　：　ＰＣ待受ＪＯＢ                    *
*    モジュール名　　　　：　ＰＣ待受ＪＯＢ　終了キーセット    *
*    作成日／更新日　　　：　2001/02/01                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　パラメタの制御キー情報をファイルに*
*                            セットする。                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SPC0030B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         01/02/01.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信確認Ｆ
     SELECT  PCCHK1    ASSIGN    TO        DA-01-S-PCCHK1
                       FILE   STATUS   IS  PCC-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信確認Ｆ
****************************************************************
 FD  PCCHK1            BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
*
 01  PCC-REC.
     03  PCC-F01                  PIC  9(08).
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  PCC-ST                   PIC  X(02).
*ＦＬＧ領域
 01  FLG-AREA.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PCC-ERR           PIC N(15) VALUE
         NC"受信確認ファイルエラー".
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
*パラメタ取得
 01  LINK-CORD             PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-CORD.
**************************************************************
 DECLARATIVES.
 PCC-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PCCHK1.
     MOVE        PCC-ST    TO        E-ST.
     MOVE        "PCCHK1  " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PCC-ERR   UPON      CONS.
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
     OPEN      OUTPUT  PCCHK1.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     MOVE   SPACE   TO   PCC-REC.
     INITIALIZE          PCC-REC.
     MOVE LINK-CORD TO   PCC-F01.
     WRITE PCC-REC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     PCCHK1.
*
 END-EXIT.
     EXIT.
*****************<<  SPC0030B   END PROGRAM  >>******************
