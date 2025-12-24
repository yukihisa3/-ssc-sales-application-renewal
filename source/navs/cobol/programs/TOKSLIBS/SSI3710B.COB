***********************************************************
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：支払ＣＳＶデータ分割　　　　　　   *
*    作成日／更新日　：2013/06/12                         *
*    作成者／更新者　：ＮＡＶ　　                         *
*    処理概要　　　　：                                   *
*      支払ＣＳＶデータＦを、１７０以下以上で分割する。　 *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSI3710B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2013/06/12.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受領累積ファイル
     SELECT  SITGKF90
       ASSIGN        TO  DA-01-S-SITGKF90
       FILE  STATUS  IS  SIH-ST.
*分割支払ＣＳＶデータ１
     SELECT  NAFSIHA1
       ASSIGN        TO  DA-01-VS-NAFSIHA1
       ACCESS MODE   IS  SEQUENTIAL
       FILE STATUS   IS  SI1-ST.
*分割支払ＣＳＶデータ２
     SELECT  NAFSIHA2
       ASSIGN        TO  DA-01-VS-NAFSIHA2
       ACCESS MODE   IS  SEQUENTIAL
       FILE STATUS   IS  SI2-ST.
*
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
******************************************************************
*    支払明細ファイル
******************************************************************
 FD  SITGKF90         BLOCK     CONTAINS  20   RECORDS
                      LABEL     RECORD    IS   STANDARD.
                      COPY      SITGKF90  OF   XFDLIB
                      JOINING   SIH       AS   PREFIX.
****************************************************************
*    分割支払ＣＳＶデータ１
****************************************************************
 FD  NAFSIHA1         LABEL     RECORD    IS   STANDARD.
                      COPY      NAFSIHAF  OF   XFDLIB
                      JOINING   SI1       AS   PREFIX.
****************************************************************
*    分割支払ＣＳＶデータ２
****************************************************************
 FD  NAFSIHA2         LABEL     RECORD    IS   STANDARD.
                      COPY      NAFSIHAF  OF   XFDLIB
                      JOINING   SI2       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SIH-ST             PIC  X(02).
     03  SI1-ST             PIC  X(02).
     03  SI2-ST             PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SIH-ERR            PIC N(15) VALUE
         NC"支払明細ファイルエラー".
     03  SI1-ERR            PIC N(15) VALUE
         NC"分割支払ＣＳＶデータ１エラー".
     03  SI2-ERR            PIC N(15) VALUE
         NC"分割支払ＣＳＶデータ２エラー".
*読込フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  IN-CNT             PIC  9(07)  VALUE ZERO.
     03  OT-CNT1            PIC  9(07)  VALUE ZERO.
     03  OT-CNT2            PIC  9(07)  VALUE ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME             PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE             PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST               PIC  9(02).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 SIH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SITGKF90.
     MOVE        SIH-ST    TO        E-ST.
     MOVE        "SITGKF90" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SIH-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SI1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NAFSIHA1.
     MOVE        SI1-ST     TO       E-ST.
     MOVE        "NAFSIHA1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SI1-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SI2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NAFSIHA2.
     MOVE        SI2-ST     TO       E-ST.
     MOVE        "NAFSIHA2" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SI2-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
* ファイルのＯＰＥＮ
     OPEN  INPUT  SITGKF90.
     OPEN  OUTPUT NAFSIHA1.
     OPEN  OUTPUT NAFSIHA2.
*
     PERFORM  RD-INF-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*  入力ファイル読込み処理                            1.1       *
****************************************************************
 RD-INF-SEC              SECTION.
*
     READ  SITGKF90
       AT  END
         MOVE  "END"        TO  END-FLG
         GO TO  RD-INF-EXIT
     END-READ.
*
     ADD  1   TO  IN-CNT.
*
     IF  IN-CNT(4:4)  =  "0000" OR "5000"
         DISPLAY "RD-CNT = " IN-CNT UPON CONS
     END-IF.
*
 RD-INF-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC              SECTION.
*
     MOVE  "MAIN-SEC"       TO  S-NAME.
*レコード作成
     IF  SIH-F03  <  171
         MOVE   SPACE           TO  SI1-REC
         INITIALIZE                 SI1-REC
         MOVE   SIH-REC         TO  SI1-REC
         WRITE  SI1-REC
         ADD    1               TO  OT-CNT1
     ELSE
         MOVE   SPACE           TO  SI2-REC
         INITIALIZE                 SI2-REC
         MOVE   SIH-REC         TO  SI2-REC
         WRITE  SI2-REC
         ADD    1               TO  OT-CNT2
     END-IF.
*
     PERFORM  RD-INF-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.

     DISPLAY  "SITGKF90 IN  = "  IN-CNT  UPON CONS.
     DISPLAY  "NAFSIHA1 OUT = "  OT-CNT1 UPON CONS.
     DISPLAY  "NAFSIHA2 OUT = "  OT-CNT2 UPON CONS.
* ファイルのＯＰＥＮ
     CLOSE  SITGKF90.
     CLOSE  NAFSIHA1.
     CLOSE  NAFSIHA2.

 END-EXIT.
     EXIT.
*****************<<  SSI3710B   END PROGRAM  >>******************
