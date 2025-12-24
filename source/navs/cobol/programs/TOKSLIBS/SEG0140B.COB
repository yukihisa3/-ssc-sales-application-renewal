****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営業所データ連携                  *
*    業務名　　　　　　　：　営業所データ連携                  *
*    モジュール名　　　　：　受信結果更新                      *
*    作成日／更新日　　　：　2000/04/11                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SEG0140B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         00/04/11.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信結果ファイル
     SELECT  RCVKEKA   ASSIGN    TO        DA-01-S-RCVKEKA
                       FILE      STATUS    RCV-ST.
*受信結果マスタ
     SELECT  EGYKEKF   ASSIGN    TO        DA-01-VI-EGYKEKL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KEK-F01
                       FILE      STATUS    KEK-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信結果ファイル                                   *
****************************************************************
 FD  RCVKEKA
                       LABEL     RECORD    IS   STANDARD.
                       COPY      EGYKEKF   OF   XFDLIB
                       JOINING   RCV       AS   PREFIX.
*
****************************************************************
*    FILE = 実行結果マスタ                                     *
****************************************************************
 FD  EGYKEKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      EGYKEKF   OF   XFDLIB
                       JOINING   KEK       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  RCV-ST                   PIC  X(02).
     03  KEK-ST                   PIC  X(02).
*ワークエリア
 01  WORK-AREA.
     03  EGYKEKF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  RCV-ERR           PIC N(15) VALUE
                        NC"受信結果Ｆエラー　".
     03  KEK-ERR           PIC N(15) VALUE
                        NC"受信結果Ｍエラー　".
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
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 RCV-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE RCVKEKA.
     MOVE        RCV-ST    TO        E-ST.
     MOVE        "RCVKEKA" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     RCV-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KEK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EGYKEKF.
     MOVE        KEK-ST    TO        E-ST.
     MOVE        "EGYKEKF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***  ファイルのオープン
     OPEN      INPUT    RCVKEKA.
     OPEN      I-O      EGYKEKF.
***  実行結果ファイル読込み
     READ      RCVKEKA  AT  END
               DISPLAY "ｼﾞｭｼﾝｹﾝｽｳﾌｧｲﾙ AT END" UPON CONS
               STOP RUN
     END-READ.
*
     MOVE      RCV-F01        TO  KEK-F01.
     READ      EGYKEKF
               INVALID
                 MOVE   "INV"   TO  EGYKEKF-INV-FLG
               NOT INVALID
                 MOVE   SPACE   TO  EGYKEKF-INV-FLG
     END-READ.
     IF        EGYKEKF-INV-FLG = "INV"
               MOVE   SPACE   TO  KEK-REC
               INITIALIZE         KEK-REC
               MOVE   RCV-REC TO  KEK-REC
               WRITE  KEK-REC
     ELSE
               MOVE   RCV-F02 TO  KEK-F02
               MOVE   RCV-F03 TO  KEK-F03
               MOVE   RCV-F04 TO  KEK-F04
               REWRITE KEK-REC
     END-IF.
***  ファイルのクローズ
     CLOSE     RCVKEKA.
     CLOSE     EGYKEKF.
***  プログラム終了
     STOP  RUN.
 PROCESS-EXD.
     EXIT.
*****************<<  SEG0140B   END PROGRAM  >>******************
