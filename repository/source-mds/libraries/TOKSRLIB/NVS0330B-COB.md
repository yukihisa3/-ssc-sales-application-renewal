# NVS0330B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0330B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　：　（株）サカタのタネ殿　　　　　　　　　　*
*    業務名　　　　：　基幹　　　　　　　　　　　　　　        *
*    サブシステム　：　Ｄ３６５連携　　　　　　　　　　        *
*    モジュール名　：　商品名称Ｍ、ＳＵＢ　２０分類セット　　  *
*    作成日／作成者：　2022/03/11 T.TAKAHASHI                  *
*    処理概要　　　：　２０分類変更ＤＴを読み、商品名称Ｍ、Ｓ  *
*                      ＵＢ商品名称Ｍを読み、存在した場合、新  *
*                      ２０分類を更新する。　　　　　　　　　  *
*    更新履歴                                                  *
*    更新日／更新者：　                                        *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVS0330B.
*                  流用:NVS0320B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/03/11.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 変更商品データ　 >>----*
     SELECT   HENMEIF   ASSIGN              DA-01-VI-HENMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEN-F011
                                            HEN-F0121
                                            HEN-F0122
                                            HEN-F0123
                        FILE      STATUS    HEN-STATUS.
*----<< 商品名称マスタ　 >>----*
     SELECT   HMEIMS    ASSIGN              DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*----<< SUB商品名称マスタ　 >>----*
     SELECT   SUBMEIL1  ASSIGN              DA-01-VI-SUBMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SUB-F011
                                            SUB-F0121
                                            SUB-F0122
                                            SUB-F0123
                        FILE      STATUS    SUB-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    変更商品データ　
******************************************************************
 FD  HENMEIF    LABEL RECORD   IS   STANDARD.
     COPY      HENMEIF         OF   XFDLIB
     JOINING   HEN                 PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS    LABEL RECORD   IS   STANDARD.
     COPY      HMEIMS           OF   XFDLIB
     JOINING   MEI                 PREFIX.
******************************************************************
*    SUB商品名称マスタ
******************************************************************
 FD  SUBMEIL1  LABEL RECORD   IS   STANDARD.
     COPY      SUBMEIL1         OF   XFDLIB
     JOINING   SUB                 PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  MEI-CNT             PIC  9(08)     VALUE  ZERO.
     03  SUB-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  HMEIMS-INV-FLG      PIC  X(03)     VALUE  SPACE.
     03  SUBMEIL1-INV-FLG    PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*
 01  WK-ST.
     03  HEN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  SUB-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVS0330B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0330B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0330B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HENMEIF.
     MOVE      "HENMEIL1 "   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1   "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBMEIL1.
     MOVE      "SUBMEIL1"   TO   AB-FILE.
     MOVE      SUB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     HENMEIF.
     OPEN     I-O       HMEIMS  SUBMEIL1.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*   システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*    変更名称データ読込
     PERFORM HENMEIF-READ-SEC.
     DISPLAY "-------------------------------------" UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変更名称データ読込　　　　　　　　　　　　　　　*
****************************************************************
 HENMEIF-READ-SEC     SECTION.
*
     MOVE    "HENMEIF-REAFD-SEC"     TO  S-NAME.
*
     READ     HENMEIF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  HENMEIF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     IF  READ-CNT(6:3) = "000" OR "500"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 HENMEIF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
 MAIN001.
*    商品名称マスタ検索
     MOVE     HEN-F011       TO  MEI-F011.
     MOVE     HEN-F0121      TO  MEI-F0121.
     MOVE     HEN-F0122      TO  MEI-F0122.
     MOVE     HEN-F0123      TO  MEI-F0123.
     READ     HMEIMS
         INVALID
              ADD    1       TO  SKIP-CNT
              GO             TO  MAIN020
     END-READ.
*
     MOVE     HEN-F02        TO  MEI-F09.
     MOVE     SYS-DATEW      TO  MEI-F99.
     REWRITE  MEI-REC.
     ADD      1              TO  MEI-CNT.
*
*
 MAIN020.
*    SUB商品名称マスタ検索
     MOVE     HEN-F011       TO  SUB-F011.
     MOVE     HEN-F0121      TO  SUB-F0121.
     MOVE     HEN-F0122      TO  SUB-F0122.
     MOVE     HEN-F0123      TO  SUB-F0123.
     READ     SUBMEIL1
         INVALID
              ADD    1       TO  SKIP2-CNT
              GO             TO  MAIN030
     END-READ.
*
     MOVE     HEN-F02        TO  SUB-F09.
     MOVE     "2920"         TO  SUB-D96.
     MOVE     "NV"           TO  SUB-D97.
     MOVE     SYS-DATEW      TO  SUB-D98.
     MOVE     WK-TIME(1:6)   TO  SUB-D99.
     REWRITE  SUB-REC.
     ADD      1              TO  SUB-CNT.
 MAIN030.
*    移行商品データ　読込み
     PERFORM HENMEIF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "-------------------------------------" UPON CONS.
     DISPLAY NC"変更商品マスタ" ":HENMEIL1  "         UPON CONS.
     DISPLAY NC"　　　　　　　" " IN  = " READ-CNT   UPON CONS.
     DISPLAY NC"　商品名称更新" ":HMEIMS    "        UPON CONS.
     DISPLAY NC"　　　　　　　" " UPD = " MEI-CNT    UPON CONS.
     DISPLAY NC"　　　対象無し" ":HMEIMS    "        UPON CONS.
     DISPLAY NC"　　　　　　　" " SKIP= " SKIP-CNT   UPON CONS.
     DISPLAY NC"ＳＵＢ商品名称" ":SUBMEIL1 "         UPON CONS.
     DISPLAY NC"　　　　　　　" " UPD = " SUB-CNT    UPON CONS.
     DISPLAY NC"　　　対象無し" ":SUBMEIL1 "         UPON CONS.
     DISPLAY NC"　　　　　　　" " SKIP= " SKIP2-CNT  UPON CONS.
     DISPLAY "-------------------------------------" UPON CONS.
*
     CLOSE     HENMEIF  HMEIMS  SUBMEIL1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
