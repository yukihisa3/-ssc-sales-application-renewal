****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＨＴ_卸業務　　　　　　　　　　*
*    モジュール名　　　　：　_卸結果→_卸確定更新　　　　　　*
*    作成日／更新日　　　：　2021/03/17                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　_卸結果データより、_卸確定データ*
*                          を作成、更新を行なう。　　。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0170B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< _卸結果集計データ　　>>*******************************
     SELECT   GKTANAF   ASSIGN    TO        DA-01-VI-GKTANAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   GKT-F01
                                                 GKT-F02
                                                 GKT-F03
                                                 GKT-F04
                        FILE      STATUS    IS   GKT-STATUS.
****<< _卸確定データ　　　　 >>******************************
     SELECT   KKTANAF  ASSIGN    TO         DA-01-VI-KKTANAL3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KKT-F04
                                                 KKT-F05
                                                 KKT-F06X
                                                 KKT-F15
                        FILE      STATUS    IS   KKT-STATUS.
****<< 未突合分_卸結果データ >>******************************
     SELECT   MMTANAF  ASSIGN    TO         DA-01-VI-MMTANAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MMT-F01
                                                 MMT-F02
                                                 MMT-F03
                                                 MMT-F04
                        FILE      STATUS    IS   MMT-STATUS.
****<< 倉庫マスタ　　　　　　 >>******************************
     SELECT   ZSOKMS   ASSIGN    TO         DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
****<< 条件ファイル　　　　　 >>******************************
     SELECT   HJYOKEN  ASSIGN    TO         DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< _卸確定データ　　　　>>*******************************
 FD  GKTANAF.
     COPY     GKTANAF   OF        XFDLIB
              JOINING   GKT       PREFIX.
****<< _卸確定データ　　　　 >>******************************
 FD  KKTANAF.
     COPY     KKTANAF   OF        XFDLIB
              JOINING   KKT       PREFIX.
****<< 未突合分_卸結果データ >>******************************
 FD  MMTANAF.
     COPY     MMTANAF   OF        XFDLIB
              JOINING   MMT       PREFIX.
****<< 倉庫マスタ　　　　　　 >>******************************
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
****<< 条件ファイル　　　　　 >>******************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 GKT-STATUS           PIC  X(02).
     02 KKT-STATUS           PIC  X(02).
     02 MMT-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0170B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  KKTANAF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  MMTANAF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  ZSOKMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  WK-SIME-DATE            PIC  9(08)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  WT-CNT              PIC  9(07)   VALUE  0.
     03  RW-CNT              PIC  9(07)   VALUE  0.
     03  SKIP-CNT            PIC  9(07)   VALUE  0.
     03  MM-CNT              PIC  9(07)   VALUE  0.
     03  MR-CNT              PIC  9(07)   VALUE  0.
     03  ER-CNT              PIC  9(07)   VALUE  0.
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD8         PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-BUMON              PIC  X(04).
 01  PARA-TANCD              PIC  X(02).
************************************************************
 PROCEDURE              DIVISION USING PARA-BUMON PARA-TANCD.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  GKTANAF.
     MOVE   "GKTANAL1"        TO    ERR-FL-ID.
     MOVE    GKT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  KKTANAF.
     MOVE   "KKTANAL3"        TO    ERR-FL-ID.
     MOVE    KKT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MMTANAF.
     MOVE   "MMTANAL1"        TO    ERR-FL-ID.
     MOVE    KKT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS1 "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "JYOKEN1 "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         INPUT     GKTANAF  ZSOKMS   HJYOKEN.
     OPEN         I-O       KKTANAF  MMTANAF.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*
     MOVE    99             TO   JYO-F01.
     MOVE    "TANA"         TO   JYO-F02.
     PERFORM HJYOKEN-READ-SEC.
     IF   HJYOKEN-INV-FLG = "INV"
          DISPLAY NC"＃＃条件Ｆ_卸日未設定！！＃＃" UPON CONS
          MOVE    4000      TO   PROGRAM-STATUS
          STOP  RUN
     ELSE
          MOVE    JYO-F04   TO   WK-SIME-DATE
          DISPLAY NC"＃_卸日" " = " WK-SIME-DATE UPON CONS
     END-IF
*
     PERFORM  GKTANAF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
           MOVE   4000    TO    PROGRAM-STATUS
     END-IF.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*_卸確定データ作成　　
     MOVE    GKT-F02              TO   KKT-F04.
     MOVE    GKT-F05              TO   KKT-F05.
     MOVE    GKT-F06              TO   KKT-F06X.
     MOVE    GKT-F03              TO   KKT-F15.
     PERFORM KKTANAF-READ-SEC.
     IF   KKTANAF-INV-FLG  =  "INV"
          MOVE  SPACE             TO   MMT-REC
          INITIALIZE                   MMT-REC
          MOVE  GKT-REC           TO   MMT-REC
          PERFORM  MMTANAF-READ-SEC
          IF  KKTANAF-INV-FLG = "INV"
              WRITE   MMT-REC
              ADD     1                TO   MM-CNT
          ELSE
              REWRITE   MMT-REC
              ADD     1                TO   MR-CNT
          END-IF
          GO                           TO   MAIN-010
     END-IF.
*
     MOVE     GKT-F07                  TO   KKT-F11.
     MOVE     GKT-F09                  TO   KKT-F89.
     MOVE     GKT-F10                  TO   KKT-F90.
     MOVE     GKT-F11                  TO   KKT-F91.
     MOVE     SYS-DATE                 TO   KKT-F96.
     MOVE     WK-TIME(1:6)             TO   KKT-F97.
     MOVE     PARA-BUMON               TO   KKT-F98.
     MOVE     PARA-TANCD               TO   KKT-F99.
     REWRITE  KKT-REC.
     ADD      1                        TO   WT-CNT.
*
 MAIN-010.
     PERFORM  GKTANAF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    _卸結果集計データ　読込　　　　　　　　　　　　
************************************************************
 GKTANAF-READ-SEC                   SECTION.
*
     READ    GKTANAF
        AT   END
             MOVE      "END"     TO   END-FLG
             GO                  TO   GKTANAF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3)  =  "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*_卸結果集計データのエラー区分＝１の時対象外
     IF  GKT-F94  =  "1"
         ADD     1               TO   ER-CNT
         GO                      TO   GKTANAF-READ-SEC
     END-IF.
*倉庫ＣＤ存在チェック
     MOVE    GKT-F02             TO   SOK-F01.
     PERFORM  ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG = "INV"
         DISPLAY NC"＃＃倉庫ＮＧ！！" "SOK-F01 = " SOK-F01
                 UPON CONS
         GO                      TO   ZSOKMS-READ-SEC
     END-IF.
*_卸日チェック
     IF  WK-SIME-DATE NOT =  GKT-F01
         GO                      TO   ZSOKMS-READ-SEC
     END-IF.
*
 GKTANAF-READ-EXT.
     EXIT.
************************************************************
*    _卸確定データ（存在チェック）　　　　　　
************************************************************
 KKTANAF-READ-SEC                   SECTION.
*
     READ    KKTANAF
        INVALID
             MOVE      "INV"     TO   KKTANAF-INV-FLG
        NOT  INVALID
             MOVE      SPACE     TO   KKTANAF-INV-FLG
     END-READ.
*
 KKTANAF-READ-EXT.
     EXIT.
************************************************************
*    未突合分_卸結果データ（存在チェック）　　
************************************************************
 MMTANAF-READ-SEC                   SECTION.
*
     READ    MMTANAF
        INVALID
             MOVE      "INV"     TO   MMTANAF-INV-FLG
        NOT  INVALID
             MOVE      SPACE     TO   MMTANAF-INV-FLG
     END-READ.
*
 MMTANAF-READ-EXT.
     EXIT.
************************************************************
*   倉庫マスタ（存在チェック）　　　　　　
************************************************************
 ZSOKMS-READ-SEC                    SECTION.
*
     READ    ZSOKMS
        INVALID
             MOVE      "INV"     TO   ZSOKMS-INV-FLG
        NOT  INVALID
             MOVE      SPACE     TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXT.
     EXIT.
************************************************************
*    条件ファイル（存在チェック）　　　　　　
************************************************************
 HJYOKEN-READ-SEC                   SECTION.
*
     READ    HJYOKEN
        INVALID
             MOVE      "INV"     TO   HJYOKEN-INV-FLG
        NOT  INVALID
             MOVE      SPACE     TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             GKTANAF  KKTANAF  MMTANAF
                       ZSOKMS   HJYOKEN.
*
     DISPLAY "* GKTANAF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* KKTANAF (WT-CNT) = " WT-CNT   " *"  UPON CONS.
     DISPLAY "* MMTANAF (MM-CNT) = " MM-CNT   " *"  UPON CONS.
     DISPLAY "* MMTANAF (MR-CNT) = " MR-CNT   " *"  UPON CONS.
     DISPLAY "* GKTANAF (ERROR)  = " ER-CNT   " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
