# SZA0181B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SZA0181B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫マスタ月次繰越                *
*    作成日／更新日　　　：　00/05/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品在庫マスタの月次繰越処理を　　*
*                          行う                                *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0181B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 在庫マスタ >>************************************
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F021
                                                 ZAI-F022
                                                 ZAI-F031
                                                 ZAI-F032
                                                 ZAI-F033
                        FILE      STATUS    IS   ZAI-STATUS.
****<< 在庫マスタ（実績） >>****************************
     SELECT   ZAMJISF   ASSIGN    TO        DA-01-VI-ZAMJISL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   ZAJ-F01
                                                 ZAJ-F021
                                                 ZAJ-F022
                                                 ZAJ-F031
                                                 ZAJ-F032
                                                 ZAJ-F033
                        FILE      STATUS    IS   ZAJ-STATUS.
****<< 条件ファイル >>**********************************
     SELECT   JYOK      ASSIGN    TO        DA-01-VI-JYOKEN1
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
****<< 在庫マスタ >>***************************************
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
****<< 在庫マスタ（実績） >>*******************************
 FD  ZAMJISF.
     COPY     ZAMJISF   OF        XFDLIB
              JOINING   ZAJ       PREFIX.
****<< 条件ファイル >>*************************************
 FD  JYOK.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(02).
     02 ZAJ-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
****  フラグ                  ****
*01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  SHORI-F                 PIC  9(02).
 01  ERR-FLG                 PIC  X(03).
 01  ZAMZAIF-END             PIC  X(03).
****  カウント                ****
 01  ZAI-U-CNT               PIC  9(07)  VALUE  0.
 01  ZAJ-U-CNT               PIC  9(07)  VALUE  0.
 01  ZAI-A-CNT               PIC  9(07)  VALUE  0.
 01  ZAJ-A-CNT               PIC  9(07)  VALUE  0.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYS-YMD             PIC  9(8).
*
 01  WORK-AREA.
     03  WK-SIMEMM           PIC  9(09)V99.
     03  WK-SIMEMM-R         REDEFINES      WK-SIMEMM.
         05  SIME-YY         PIC  9(07).
         05  SIME-MM         PIC  9(02).
         05  SIME-SS         PIC  9(02).
     03  IX                  PIC  9(02).
*
 01  WK-ZAI-KEY.
     03  WK-ZAI-F01          PIC  X(02).
     03  WK-ZAI-F02          PIC  X(16).
     03  WK-ZAI-F03          PIC  X(06).
 01  WK-ZAJ-KEY.
     03  WK-ZAJ-F01          PIC  X(02).
     03  WK-ZAJ-F02          PIC  X(16).
     03  WK-ZAJ-F03          PIC  X(06).
*
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0181B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMZAIF.
     MOVE   "ZAMZAIF "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMJISF.
     MOVE   "ZAMJISF "        TO    ERR-FL-ID.
     MOVE    ZAJ-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JYOK.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           INIT-SEC.
     PERFORM           MAIN-SEC
                       UNTIL     WK-ZAI-KEY  =  HIGH-VALUE
                       AND       WK-ZAJ-KEY  =  HIGH-VALUE.
     PERFORM           END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC                        SECTION.
     OPEN         I-O       ZAMZAIF    ZAMJISF.
     OPEN         INPUT     JYOK.
*システム日付の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     ACCEPT   LINK-IN-YMD6      FROM   DATE.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-YMD.
*
     PERFORM      JYO-SYORI-SEC.
*
     MOVE     "6M"                TO   ZAJ-F01   ZAI-F01.
     MOVE     "00368650"          TO   ZAJ-F021  ZAI-F021.
     MOVE     "     21 "          TO   ZAJ-F022  ZAI-F022.
     MOVE     "A00"               TO   ZAJ-F031  ZAI-F031.
     MOVE     "7"                 TO   ZAJ-F032  ZAI-F032.
     MOVE     "15"                TO   ZAJ-F033  ZAI-F033.
*
     START   ZAMJISF      KEY    >=   ZAJ-F01
                                      ZAJ-F021 ZAJ-F022
                                      ZAJ-F031 ZAJ-F032 ZAJ-F033
         INVALID   MOVE  HIGH-VALUE   TO  WK-ZAJ-KEY
                   GO                 TO  INIT-SEC-EXIT
     END-START.
*
     START   ZAMZAIF      KEY    >=   ZAI-F01
                                      ZAI-F021 ZAI-F022
                                      ZAI-F031 ZAI-F032 ZAI-F033
         INVALID   MOVE  HIGH-VALUE   TO  WK-ZAJ-KEY
                   GO                 TO  INIT-SEC-EXIT
     END-START.
*
     PERFORM      ZAI-READ-SEC.
     PERFORM      ZAJ-READ-SEC.
     DISPLAY "ZAI-KEY = " ZAI-F01 ZAI-F02 ZAI-F03 UPON CONS.
     DISPLAY "ZAJ-KEY = " ZAJ-F01 ZAJ-F02 ZAJ-F03 UPON CONS.
*
 INIT-SEC-EXIT.
     EXIT.
*==========================================================*
*      1.1      条件ファイル処理                           *
*==========================================================*
 JYO-SYORI-SEC                   SECTION.
     MOVE         99             TO   JYO-F01.
     MOVE        "ZAI"           TO   JYO-F02.
     READ    JYOK
        INVALID  KEY
             MOVE      HIGH-VALUE     TO  WK-ZAI-KEY
                                          WK-ZAJ-KEY
        NOT INVALID  KEY
             MOVE      JYO-F05        TO  WK-SIMEMM
     END-READ.
**** DISPLAY  "ｼﾒﾋﾞ=" SIME-YY "/" SIME-MM  UPON CONS.
*
     IF  SIME-MM  <    1    OR
         SIME-MM  >   12
             MOVE      HIGH-VALUE     TO  WK-ZAI-KEY
                                          WK-ZAJ-KEY
             GO       TO             JYO-SYORI-EXIT
     END-IF.
     IF  SIME-MM  >    4    AND
         SIME-MM  <   12
         COMPUTE  IX   =    SIME-MM   -   5     +    1
     ELSE
         IF  SIME-MM    =   12
             MOVE       8         TO   IX
         ELSE
             COMPUTE   IX    =    SIME-MM   +    8
         END-IF
     END-IF.
 JYO-SYORI-EXIT.
     EXIT.
************************************************************
*      2.0      主処理　                                   *
************************************************************
 MAIN-SEC                        SECTION.
*
     EVALUATE  TRUE
        WHEN   WK-ZAI-KEY  =  WK-ZAJ-KEY
                      PERFORM     ZAJ-UPDATE-SEC
                      PERFORM     ZAI-UPDATE-SEC
        WHEN   WK-ZAI-KEY  <  WK-ZAJ-KEY
                      PERFORM     ZAJ-WRITE-SEC
                      PERFORM     ZAI-UPDATE-SEC
        WHEN   WK-ZAI-KEY  >  WK-ZAJ-KEY
                      PERFORM     ZAI-WRITE-SEC
     END-EVALUATE.
*
     IF    WK-ZAI-KEY   NOT =   HIGH-VALUE
                      PERFORM     ZAI-READ-SEC
     END-IF.
     IF    WK-ZAJ-KEY   NOT =   HIGH-VALUE
                      PERFORM     ZAJ-READ-SEC
     END-IF.
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      2.1      在庫マスタ・繰り越し                       *
************************************************************
 ZAI-UPDATE-SEC                  SECTION.
*当月→前月
     MOVE    ZAI-F05             TO   ZAI-F17.
     MOVE    ZAI-F07             TO   ZAI-F15.
     MOVE    ZAI-F08             TO   ZAI-F16.
     COMPUTE ZAI-F05  =   ZAI-F04  -  ZAI-F11  +  ZAI-F12.
*次月→当月
     COMPUTE ZAI-F06  =   ZAI-F11  -  ZAI-F12.
     MOVE    ZAI-F13             TO   ZAI-F09.
     MOVE    ZAI-F14             TO   ZAI-F10.
     MOVE    ZAI-F11             TO   ZAI-F07.
     MOVE    ZAI-F12             TO   ZAI-F08.
*次月クリア
     MOVE    ZERO                TO   ZAI-F11
                                      ZAI-F12
                                      ZAI-F13
                                      ZAI-F14.
     MOVE    ZERO                TO   ZAI-F20.
*
     REWRITE ZAI-REC.
     ADD     1                   TO   ZAI-U-CNT.
 ZAI-UPDATE-EXIT.
     EXIT.
************************************************************
*      2.2      在庫マスタ（実績）・繰り越し               *
************************************************************
 ZAJ-UPDATE-SEC                  SECTION.
     MOVE    ZAI-F09             TO   ZAJ-F051 (IX).
     MOVE    ZAI-F07             TO   ZAJ-F052 (IX).
     MOVE    ZAI-F08             TO   ZAJ-F053 (IX).
*
     REWRITE ZAJ-REC.
     ADD     1                   TO   ZAJ-U-CNT.
 ZAJ-UPDATE-EXIT.
     EXIT.
************************************************************
*      2.3      在庫マスタ・追加                           *
************************************************************
 ZAI-WRITE-SEC                   SECTION.
     MOVE    SPACE               TO   ZAI-REC.
     INITIALIZE                       ZAI-REC.
     MOVE    ZAJ-F01             TO   ZAI-F01.
     MOVE    ZAJ-F02             TO   ZAI-F02.
     MOVE    ZAJ-F03             TO   ZAI-F03.
     MOVE    SYS-YMD             TO   ZAI-F98
                                      ZAI-F99.
*****WRITE   ZAI-REC.
     ADD     1                   TO   ZAI-A-CNT.
*
     START   ZAMZAIF      KEY    >    ZAI-F01
                                      ZAI-F021 ZAI-F022
                                      ZAI-F031 ZAI-F032 ZAI-F033
         INVALID   MOVE  HIGH-VALUE   TO  WK-ZAI-KEY
     END-START.
*
 ZAI-WRITE-EXIT.
     EXIT.
************************************************************
*      2.4      在庫マスタ（実績）・追加                   *
************************************************************
 ZAJ-WRITE-SEC                   SECTION.
     MOVE    SPACE               TO   ZAJ-REC.
     INITIALIZE                       ZAJ-REC.
     MOVE    ZAI-F01             TO   ZAJ-F01.
     MOVE    ZAI-F02             TO   ZAJ-F02.
     MOVE    ZAI-F03             TO   ZAJ-F03.
     MOVE    ZAI-F09             TO   ZAJ-F051 (IX).
     MOVE    ZAI-F07             TO   ZAJ-F052 (IX).
     MOVE    ZAI-F08             TO   ZAJ-F053 (IX).
     MOVE    SYS-YMD             TO   ZAJ-F98
                                      ZAJ-F99.
     WRITE   ZAJ-REC.
     ADD     1                   TO   ZAJ-A-CNT.
*
     START   ZAMJISF      KEY    >    ZAJ-F01
                                      ZAJ-F021 ZAJ-F022
                                      ZAJ-F031 ZAJ-F032 ZAJ-F033
         INVALID   MOVE  HIGH-VALUE   TO  WK-ZAJ-KEY
     END-START.
*
 ZAJ-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                         SECTION.
     CLOSE             JYOK      ZAMZAIF    ZAMJISF.
     DISPLAY "* ZAMZAIF (UPDATE)= " ZAI-U-CNT " *" UPON CONS.
     DISPLAY "* ZAMZAIF (WRITE) = " ZAI-A-CNT " *" UPON CONS.
     DISPLAY "* ZAMJISF (UPDATE)= " ZAJ-U-CNT " *" UPON CONS.
     DISPLAY "* ZAMJISF (WRITE) = " ZAJ-A-CNT " *" UPON CONS.
 END-SEC-EXT.
     EXIT.
*==========================================================*
*            在庫マスタの読込処理
*==========================================================*
 ZAI-READ-SEC                    SECTION.
     READ    ZAMZAIF   NEXT
        AT   END
             MOVE      HIGH-VALUE     TO  WK-ZAI-KEY
             GO        TO        ZAI-READ-EXT
     END-READ.
*
     MOVE    ZAI-F01             TO   WK-ZAI-F01.
     MOVE    ZAI-F02             TO   WK-ZAI-F02.
     MOVE    ZAI-F03             TO   WK-ZAI-F03.
 ZAI-READ-EXT.
     EXIT.
*==========================================================*
*            在庫マスタの読込処理
*==========================================================*
 ZAJ-READ-SEC                    SECTION.
     READ    ZAMJISF   NEXT
        AT   END
             MOVE      HIGH-VALUE     TO  WK-ZAJ-KEY
             GO        TO        ZAJ-READ-EXT
     END-READ.
*
     MOVE    ZAJ-F01             TO   WK-ZAJ-F01.
     MOVE    ZAJ-F02             TO   WK-ZAJ-F02.
     MOVE    ZAJ-F03             TO   WK-ZAJ-F03.
 ZAJ-READ-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
