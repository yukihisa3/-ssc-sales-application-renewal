****************************************************************
*    顧客名　　　　：　（株）サカタのタネ殿　　　　　　　　　　*
*    業務名　　　　：　基幹　　　　　　　　　　　　　　        *
*    サブシステム　：　Ｄ３６５連携　　　　　　　　　　        *
*    モジュール名　：　商品変換ＴＢＬコンバート　　　　　　　  *
*    作成日／作成者：　2021/06/02 INOUE                        *
*    処理概要　　　：　基幹システムマスタより該当データ　      *
*                      を編集・抽出する　　　　　　　　　      *
*    更新履歴                                                  *
*    更新日／更新者：　                                        *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            CNVTOSTM.
*                  流用:D365CNV3
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/02.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 商品変換ＴＢＬ　 >>----*
     SELECT   SHOTBL1   ASSIGN              DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE      STATUS    TBL-STATUS.
*----<< SUB商品名称マスタ　 >>----*
     SELECT   SUBMEIL1  ASSIGN              DA-01-VI-SUBMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*----<< 移行商品データ　 >>----*
     SELECT   CNVTOST1  ASSIGN              DA-01-VI-CNVTOST1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IN-F01
                                            IN-F02
                        FILE      STATUS    IN-STATUS.
*----<< ＳＵＢ変換ＴＢＬ >>----*
     SELECT   SUBTBLF   ASSIGN              DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HTB-F01
                                            HTB-F02
                        FILE      STATUS    HTB-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    移行商品データ　
******************************************************************
 FD  CNVTOST1  LABEL RECORD   IS   STANDARD.
     COPY      CNVTOST1       OF   XFDLIB
     JOINING   IN                  PREFIX.
******************************************************************
*    商品変換ＴＢＬ　
******************************************************************
 FD  SHOTBL1   LABEL RECORD   IS   STANDARD.
     COPY      SHOTBL1        OF   XFDLIB
     JOINING   TBL                 PREFIX.
******************************************************************
*    SUB商品名称マスタ
******************************************************************
 FD  SUBMEIL1  LABEL RECORD   IS   STANDARD.
     COPY      SUBMEIL1         OF   XFDLIB
     JOINING   MEI                 PREFIX.
******************************************************************
*    SUB商品変換ＴＢＬ
******************************************************************
 FD  SUBTBLF   LABEL RECORD   IS   STANDARD.
     COPY      SUBTBLL1         OF   XFDLIB
     JOINING   HTB                 PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT1           PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT2           PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT3           PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT4           PIC  9(08)     VALUE  ZERO.
     03  TBL-CNT             PIC  9(08)     VALUE  ZERO.
     03  HTB-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  CRTTBLF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  CNVTOST1-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHOTBL1-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SUBMEIF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SUBTBLF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
*退避
 01  WK-TBL-REC              PIC  X(085)    VALUE  SPACE.
 01  WK-HTB-REC              PIC  X(500)    VALUE  SPACE.
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
     03  MEI-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  CRT-STATUS        PIC  X(02).
     03  CRS-STATUS        PIC  X(02).
     03  NON-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  HTB-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "CNVTOSTM".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "CNVTOSTM".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "CNVTOSTM".
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
*LINKAGE                SECTION.
*01  PARA-IN-JDATE             PIC   9(08).
*01  PARA-IN-JTIME             PIC   9(04).
*01  PARA-IN-TORICD            PIC   9(08).
*01  PARA-IN-SOKO              PIC   X(02).
*01  PARA-IN-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CNVTOST1.
     MOVE      "CNVTOST1 "   TO   AB-FILE.
     MOVE      IN-STATUS     TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
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
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBTBLF.
     MOVE      "SUBTBLL1"    TO   AB-FILE.
     MOVE      HTB-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SUBMEIL1  CNVTOST1
     OPEN     I-O       SHOTBL1  SUBTBLF.
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
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*    ビバデータ読込み
     PERFORM CNVTOSTM-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 CNVTOSTM-READ-SEC    SECTION.
*
     MOVE    "CNVTOSTM-READ-SEC"     TO  S-NAME.
*
     READ     CNVTOST1
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO CNVTOSTM-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 CNVTOSTM-READ-EXIT.
     EXIT.
****************************************************************
*　　ＳＵＢ商品変換ＴＢＬ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 SUBTBLF-READ-SEC    SECTION.
*
     MOVE    "SUBTBLF-READ-SEC"     TO  S-NAME.
*
     READ     SUBTBLF
              INVALID      MOVE "INV"  TO    SUBTBLF-INV-FLG
              NOT  INVALID MOVE SPACE  TO    SUBTBLF-INV-FLG
     END-READ.
*
 SUBTBLF-READ-EXIT.
     EXIT.
****************************************************************
*　　商品変換ＴＢＬ読込　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SHOTBL1-READ-SEC    SECTION.
*
     MOVE    "SHOTBL1-READ-SEC"     TO  S-NAME.
*
     READ     SHOTBL1
              INVALID      MOVE "INV"  TO    SHOTBL1-INV-FLG
              NOT  INVALID MOVE SPACE  TO    SHOTBL1-INV-FLG
     END-READ.
*
 SHOTBL1-READ-EXIT.
     EXIT.
****************************************************************
*　　ＳＵＢ商品名称マスタ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 SUBMEIF-READ-SEC    SECTION.
*
     MOVE    "SUBMEIF-READ-SEC"     TO  S-NAME.
*
     MOVE     TBL-F031              TO  MEI-F011.
     MOVE     TBL-F0321             TO  MEI-F0121.
     MOVE     TBL-F0322             TO  MEI-F0122.
     MOVE     TBL-F0323             TO  MEI-F0123.
     READ     SUBMEIL1
              INVALID      MOVE "INV"  TO    SUBMEIF-INV-FLG
              NOT  INVALID MOVE SPACE  TO    SUBMEIF-INV-FLG
     END-READ.
*
 SUBMEIF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
 MAIN001.
*    商品変換ＴＢＬ索引
     MOVE     IN-F01                TO  TBL-F01.
     MOVE     IN-F02                TO  TBL-F02.
     PERFORM SHOTBL1-READ-SEC.
     IF  SHOTBL1-INV-FLG  =  "INV"
         ADD  1              TO  SKIP-CNT1
         GO                  TO  MAIN999
     ELSE
         PERFORM  SUBMEIF-READ-SEC
     END-IF.
     MOVE TBL-REC                   TO  WK-TBL-REC
     MOVE IN-F01                    TO  TBL-F01.
     MOVE IN-F03                    TO  TBL-F02
     PERFORM SHOTBL1-READ-SEC.
     IF  SHOTBL1-INV-FLG  =  "INV"
         MOVE   SPACE        TO  TBL-REC
         INITIALIZE              TBL-REC
         MOVE   WK-TBL-REC   TO  TBL-REC
         MOVE   IN-F03       TO  TBL-F02
         MOVE   SYS-DATEW    TO  TBL-F98
         MOVE   SYS-DATEW    TO  TBL-F99
         WRITE  TBL-REC
         ADD    1            TO  TBL-CNT
     ELSE
         ADD    1            TO  SKIP-CNT2
     END-IF.
*    ＳＵＢ商品変換ＴＢＬ索引
     MOVE     IN-F01                TO  HTB-F01.
     MOVE     IN-F02                TO  HTB-F02.
     PERFORM SUBTBLF-READ-SEC.
     IF  SUBTBLF-INV-FLG  =  "INV"
         ADD  1              TO  SKIP-CNT3
         GO                  TO  MAIN999
     END-IF.
     MOVE HTB-REC                   TO  WK-HTB-REC
     MOVE IN-F01                    TO  HTB-F01.
     MOVE IN-F03                    TO  HTB-F02
     PERFORM SUBTBLF-READ-SEC.
     IF  SUBTBLF-INV-FLG  =  "INV"
         MOVE   SPACE        TO  HTB-REC
         INITIALIZE              HTB-REC
         MOVE   WK-HTB-REC   TO  HTB-REC
         MOVE   IN-F03       TO  HTB-F02
         MOVE   "2920"       TO  HTB-F92  HTB-F96
         MOVE   "96"         TO  HTB-F93  HTB-F97
         MOVE   SYS-DATEW    TO  HTB-F94  HTB-F98
         MOVE   WK-TIME(1:6) TO  HTB-F95  HTB-F99
         IF   SUBMEIF-INV-FLG =  SPACE
              MOVE  MEI-D01  TO  HTB-F17
              MOVE  MEI-D02  TO  HTB-F18
         END-IF
         WRITE  HTB-REC
         ADD    1            TO  HTB-CNT
     ELSE
         ADD    1            TO  SKIP-CNT4
     END-IF.
 MAIN999.
*    移行商品データ　読込み
     PERFORM CNVTOSTM-READ-SEC.
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
     DISPLAY "READ-CNT  = " READ-CNT    UPON CONS.
     DISPLAY "TBL-CNT   = " TBL-CNT     UPON CONS.
     DISPLAY "HTB-CNT   = " HTB-CNT     UPON CONS.
     DISPLAY "SKIP-CNT1 = " SKIP-CNT1   UPON CONS.
     DISPLAY "SKIP-CNT2 = " SKIP-CNT2   UPON CONS.
     DISPLAY "SKIP-CNT3 = " SKIP-CNT3   UPON CONS.
     DISPLAY "SKIP-CNT4 = " SKIP-CNT4   UPON CONS.
*
     CLOSE     SHOTBL1   SUBMEIL1  SUBTBLF  CNVTOST1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
