****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＨＴ_卸業務　　　　　　　　　　*
*    モジュール名　　　　：　未突合_卸確定ＤＴ更新　　　　　　*
*    作成日／更新日　　　：　2021/03/18                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　未突合_卸確定ＤＴを読み、_卸確定*
*                          データの作成を行なう。　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0180B.
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
     SELECT   MMTANAF   ASSIGN    TO        DA-01-VI-MMTANAL3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   MMT-F02
                                                 MMT-F03
                                                 MMT-F05
                                                 MMT-F06
                        FILE      STATUS    IS   MMT-STATUS.
****<< _卸確定データ　　　　 >>******************************
     SELECT   KKTANAF  ASSIGN    TO         DA-01-VI-KKTANAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KKT-F01
                        FILE      STATUS    IS   KKT-STATUS.
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
****<< 未突合_卸結果データ　>>*******************************
 FD  MMTANAF.
     COPY     MMTANAF   OF        XFDLIB
              JOINING   MMT       PREFIX.
****<< _卸確定データ　　　　 >>******************************
 FD  KKTANAF.
     COPY     KKTANAF   OF        XFDLIB
              JOINING   KKT       PREFIX.
****<< 条件ファイル　　　　　 >>******************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 MMT-STATUS           PIC  X(02).
     02 KKT-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
****  ワ－ク  ***
 01  WK-AREA.
     02  WK-TANAOROSINO      PIC  9(10).
     02  WK-TANAOROSINO-R    REDEFINES   WK-TANAOROSINO.
       03  WK-GENPYONO       PIC  9(09).
       03  WK-GYONO          PIC  9(01).
     02  WK-SOUKOCD          PIC  X(02)  VALUE  SPACE.
     02  WK-BUMON                 PIC  9(09)V9(02).
     02  WK-BUMON-R               REDEFINES   WK-BUMON.
         03  WK-BUMON-CD1         PIC  9(05).
         03  WK-BUMON-CD2         PIC  9(03).
         03  WK-BUMON-CD3         PIC  9(01).
         03  WK-BUMON-CD4         PIC  9(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0180B".
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
 01  HJYOKEN-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  WK-SIME-DATE            PIC  9(08)  VALUE  ZERO.
 01  OLD-TANABAN             PIC  X(06)  VALUE  SPACE.
 01  WK-CHK-SOK              PIC  X(02)  VALUE  SPACE.
 01  TAI-FLG                 PIC  X(02)  VALUE  SPACE.
 01  WK-GYO-NO               PIC  9(02)  VALUE  ZERO.
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
                   PROCEDURE  MMTANAF.
     MOVE   "MMTANAL1"        TO    ERR-FL-ID.
     MOVE    MMT-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     MMTANAF.
     OPEN         I-O       KKTANAF.
     OPEN         I-O       HJYOKEN.
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
*_卸日取得
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
*原票ＮＯ取得
     MOVE   63               TO   JYO-F01.
     MOVE   SPACE            TO   JYO-F02.
     PERFORM HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = "INV"
          MOVE   "END"       TO   END-FLG
          DISPLAY NC"＃＃原票ＮＯ取得エラー！！＃＃" UPON CONS
          MOVE    4000       TO   PROGRAM-STATUS
          STOP  RUN
     ELSE
          MOVE   JYO-F05     TO   WK-GENPYONO
          MOVE   ZERO        TO   WK-GYONO
          DISPLAY NC"＃＃取得原票" "NO = " WK-GENPYONO UPON CONS
     END-IF.
*
     PERFORM  MMTANAF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
          DISPLAY NC"＃　未突合_卸結果デ　＃" UPON CONS
          DISPLAY NC"＃　ータは今回の処理　＃" UPON CONS
          DISPLAY NC"＃　では存在しません　＃" UPON CONS
          DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
***********MOVE   4000    TO    PROGRAM-STATUS
     END-IF.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*倉庫ＣＤブレイク
     IF  MMT-F02   NOT =   WK-SOUKOCD
         MOVE  MMT-F02            TO   WK-SOUKOCD
         MOVE  MMT-F03            TO   OLD-TANABAN
         IF  WK-GYONO  >  ZERO
             ADD  1               TO   WK-GENPYONO
             MOVE ZERO            TO   WK-GYONO  WK-GYO-NO
         END-IF
     ELSE
*********_番がブレイク時
         IF  MMT-F03(1:4)  NOT =  OLD-TANABAN(1:4)
             MOVE  MMT-F03        TO   OLD-TANABAN
             IF  WK-GYONO  >  ZERO
                 ADD       1      TO   WK-GENPYONO
                 MOVE      ZERO   TO   WK-GYONO  WK-GYO-NO
             END-IF
         END-IF
     END-IF.
*
     MOVE     SPACE                    TO   KKT-REC.
     INITIALIZE                             KKT-REC.
     MOVE     WK-TANAOROSINO           TO   KKT-F01.
     MOVE     WK-BUMON-CD2             TO   KKT-F02.
     MOVE     WK-BUMON-CD3             TO   KKT-F03.
     MOVE     MMT-F02                  TO   KKT-F04.
     MOVE     MMT-F05                  TO   KKT-F05.
     MOVE     MMT-F06(1:5)             TO   KKT-F06.
     MOVE     MMT-F06(6:2)             TO   KKT-F07.
     MOVE     MMT-F06(8:1)             TO   KKT-F08.
     MOVE     MMT-F07                  TO   KKT-F11.
     MOVE     SYS-DATE(3:2)            TO   KKT-F13.
     MOVE     MMT-F03                  TO   KKT-F15.
     MOVE     MMT-F01                  TO   KKT-F83.
     MOVE     MMT-F04                  TO   KKT-F88.
     MOVE     MMT-F09                  TO   KKT-F89.
     MOVE     MMT-F10                  TO   KKT-F90.
     MOVE     MMT-F11                  TO   KKT-F91.
     MOVE     PARA-BUMON               TO   KKT-F98.
     MOVE     PARA-TANCD               TO   KKT-F99.
     MOVE     SYS-DATE                 TO   KKT-F96.
     MOVE     WK-TIME(1:6)             TO   KKT-F97.
     ADD      1                        TO   WK-GYONO WK-GYO-NO.
*行カウントが９以上の場合、原票ＮＯカウントアップ
     IF  WK-GYO-NO  >  9
         ADD  1               TO   WK-GENPYONO
         MOVE ZERO            TO   WK-GYONO   WK-GYO-NO
     END-IF.
     PERFORM  KKTANAF-READ-SEC.
     IF  KKTANAF-INV-FLG  =  "INV"
         WRITE  KKT-REC
         ADD      1                    TO   WT-CNT
     ELSE
         DISPLAY NC"＃＃原票ＮＯ重複！！＃＃" UPON CONS
         DISPLAY NC"＃＃原票" "NO = "  WK-TANAOROSINO
         UPON CONS
     END-IF.
*
 MAIN-010.
     PERFORM  MMTANAF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    _卸結果集計データ　読込　　　　　　　　　　　　
************************************************************
 MMTANAF-READ-SEC                   SECTION.
*
     READ    MMTANAF
        AT   END
             MOVE      "END"     TO   END-FLG
             GO                  TO   MMTANAF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3)  =  "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*倉庫ＣＤブレイクチェック
     IF  MMT-F02  NOT =  WK-CHK-SOK
         MOVE     20             TO   JYO-F01
         MOVE     MMT-F02        TO   JYO-F02
         PERFORM HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG  =  "INV"
             MOVE   "NG"         TO   TAI-FLG
             DISPLAY NC"＃条件Ｆ倉庫未登録エラー１＃" UPON CONS
         ELSE
             IF  JYO-F06  =  1
                 MOVE   JYO-F07      TO   WK-BUMON
                 MOVE   "OK"         TO   TAI-FLG
             ELSE
                 MOVE   "NG"         TO   TAI-FLG
                 DISPLAY NC"＃条件Ｆ倉庫未登録エラー２＃" UPON
                 CONS
             END-IF
         END-IF
     END-IF.
*対象チェック
     IF  TAI-FLG  =  "OK"
         CONTINUE
     ELSE
         ADD          1          TO   SKIP-CNT
         GO                      TO   MMTANAF-READ-SEC
     END-IF.
*
 MMTANAF-READ-EXT.
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
*原票ＮＯ取得
     MOVE   63               TO   JYO-F01.
     MOVE   SPACE            TO   JYO-F02.
     PERFORM HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = "INV"
          MOVE   "END"       TO   END-FLG
          DISPLAY NC"＃＃原票ＮＯ取得エラー！！＃＃" UPON CONS
          MOVE    4000       TO   PROGRAM-STATUS
          STOP  RUN
     END-IF.
*原票ＮＯを更新する
     IF  IN-CNT  >  ZERO
         IF  WK-GYONO  >  ZERO
             ADD       1              TO   WK-GENPYONO
         END-IF
         MOVE  WK-GENPYONO            TO   JYO-F05
         DISPLAY NC"＃＃最終原票" "NO = " WK-GENPYONO UPON CONS
         REWRITE  JYO-REC
     END-IF.
*
     CLOSE             MMTANAF  KKTANAF  HJYOKEN.
*
     DISPLAY "* MMTANAF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* KKTANAF (WT-CNT) = " WT-CNT   " *"  UPON CONS.
     DISPLAY "* SKIP-CNT         = " SKIP-CNT " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
