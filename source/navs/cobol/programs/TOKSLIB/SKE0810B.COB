****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品システム                  *
*    業務名　　　　　　　：　出荷検品                          *
*    モジュール名　　　　：　検品情報管理Ｆ更新                *
*    作成日／更新日　　　：　01/04/16                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　検品情報管理Ｆへ送信Ｆ作成の日付・*
*                            時間を登録する。（パラにもセット）*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKE0810B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         01/04/16.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*検品情報管理Ｆ
     SELECT   KENKANF  ASSIGN    TO        DA-01-VI-KENKANL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KEN-F01
                                           KEN-F02
                                           KEN-F03
                       FILE  STATUS    IS  KEN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
******************************************************************
*    検品情報管理ファイル
******************************************************************
 FD  KENKANF           LABEL     RECORD    IS   STANDARD.
                       COPY      KENKANF   OF   XFDLIB
                       JOINING   KEN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEN-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  KENKANF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KEN-ERR           PIC N(15) VALUE
         NC"検品情報管理Ｆエラー".
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
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(04).
     03  FILLER         PIC  9(04).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE               SECTION.
 01  PARA-SOKCD             PIC   X(02).
 01  PARA-SYJOHOF           PIC   9(06).
 01  PARA-SYLABHF           PIC   9(06).
 01  PARA-SYLABMF           PIC   9(06).
 01  PARA-RCVERRF           PIC   9(06).
 01  PARA-PDATE             PIC   9(08).
 01  PARA-PTIME             PIC   9(04).
**************************************************************
 PROCEDURE             DIVISION USING PARA-SOKCD
                                      PARA-SYJOHOF
                                      PARA-SYLABHF
                                      PARA-SYLABMF
                                      PARA-RCVERRF
                                      PARA-PDATE
                                      PARA-PTIME.
**************************************************************
 DECLARATIVES.
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KENKANF.
     MOVE        KEN-ST    TO        E-ST.
     MOVE       "KENKANL1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEN-ERR   UPON      CONS.
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
     OPEN      I-O     KENKANF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
       DISPLAY "LINK-YMD8 = " LINK-OUT-YMD8 UPON CONS
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME2      FROM TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*日付・時間で検品情報管理Ｆ存在チェック
     MOVE      PARA-SOKCD    TO   KEN-F01.
     MOVE      SYS-DATEW     TO   KEN-F02.
     MOVE      SYS-TIMEW     TO   KEN-F03.
     READ      KENKANF
               INVALID  MOVE  "INV"  TO  KENKANF-INV-FLG
           NOT INVALID  MOVE  SPACE  TO  KENKANF-INV-FLG
     END-READ.
*更新チェック
     IF        KENKANF-INV-FLG = "INV"
               MOVE   SPACE        TO   KEN-REC
               INITIALIZE               KEN-REC
               MOVE   PARA-SOKCD   TO   KEN-F01
               MOVE   SYS-DATEW    TO   KEN-F02 PARA-PDATE
               MOVE   SYS-TIMEW    TO   KEN-F03 PARA-PTIME
               MOVE   PARA-SYJOHOF TO   KEN-F04
               MOVE   PARA-SYLABHF TO   KEN-F05
               MOVE   PARA-SYLABMF TO   KEN-F06
               MOVE   PARA-RCVERRF TO   KEN-F07
               WRITE  KEN-REC
     ELSE
               DISPLAY NC"＃＃既に登録有り！！＃＃" UPON CONS
               MOVE   "4000"      TO   PROGRAM-STATUS
               STOP  RUN
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     KENKANF.
*
 END-EXIT.
     EXIT.
*****************<<  SKE0810B   END PROGRAM  >>******************
