****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　部門間在庫移動ＤＴ集計　　　　　  *
*    作成日／更新日　　　：　2018/01/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　部門間移動抽出Ｆを読み、部門間移　*
*                        ：　動集計Ｆのキー単位に集計を行なう。*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBZ0020B.
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
****<< 部門間移動抽出ファイル >>******************************
     SELECT   BUMIDTF   ASSIGN    TO        DA-01-VI-BUMIDTL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   IDT-F06 IDT-F03
                                                 IDT-F05 IDT-F02
                                                 IDT-F07 IDT-F10
                                                 IDT-F04
                        FILE      STATUS    IS   IDT-STATUS.
****<< 部門間移動集計ファイル >>******************************
     SELECT   BUMSYUF   ASSIGN    TO        DA-01-VI-BUMSYUL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SYU-F01 SYU-F02
                                                 SYU-F03 SYU-F04
                                                 SYU-F05 SYU-F06
                                                 SYU-F07 SYU-F08
                        FILE      STATUS    IS   SYU-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 部門間移動抽出ファイル >>******************************
 FD  BUMIDTF.
     COPY     BUMIDTF   OF        XFDLIB
              JOINING   IDT       PREFIX.
****<< 部門間移動集計ファイル >>******************************
 FD  BUMSYUF.
     COPY     BUMSYUF   OF        XFDLIB
              JOINING   SYU       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 IDT-STATUS           PIC  X(02).
     02 SYU-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBZ0020B".
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
 01  BUMSYUF-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  RWT-CNT             PIC  9(07)   VALUE  0.
     03  CRT-CNT             PIC  9(07)   VALUE  0.
*キー退避
 01  WK-KEY.
     03  WK-SYU-F01          PIC  X(04)   VALUE  SPACE.
     03  WK-SYU-F02          PIC  X(02)   VALUE  SPACE.
     03  WK-SYU-F03          PIC  X(04)   VALUE  SPACE.
     03  WK-SYU-F04          PIC  9(08)   VALUE  ZERO.
     03  WK-SYU-F05          PIC  X(08)   VALUE  SPACE.
     03  WK-SYU-F06          PIC  X(05)   VALUE  SPACE.
     03  WK-SYU-F07          PIC  X(02)   VALUE  SPACE.
     03  WK-SYU-F08          PIC  X(01)   VALUE  SPACE.
*
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
*
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BUMIDTF.
     MOVE   "BUMIDTL1"        TO    ERR-FL-ID.
     MOVE    IDT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BUMSYUF.
     MOVE   "BUMSYUL1"        TO    ERR-FL-ID.
     MOVE    SYU-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     BUMIDTF.
     OPEN         I-O       BUMSYUF.
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
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
*
     PERFORM  BUMIDTF-READ-SEC.
*
     IF  END-FLG  =  "END"
         DISPLAY NC"＃＃処理対象データがありません！＃＃"
         UPON CONS
     END-IF.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*Z門間移動集計Ｆ索引
     PERFORM  BUMSYUF-READ-SEC.
*
     IF  BUMSYUF-INV-FLG  =  "INV"
         MOVE    SPACE           TO   SYU-REC
         INITIALIZE                   SYU-REC
         MOVE    WK-SYU-F01      TO   SYU-F01
         MOVE    WK-SYU-F02      TO   SYU-F02
         MOVE    WK-SYU-F03      TO   SYU-F03
         MOVE    WK-SYU-F04      TO   SYU-F04
         MOVE    WK-SYU-F05      TO   SYU-F05
         MOVE    WK-SYU-F06      TO   SYU-F06
         MOVE    WK-SYU-F07      TO   SYU-F07
         MOVE    WK-SYU-F08      TO   SYU-F08
         ADD     IDT-F15         TO   SYU-F09
         MOVE    SYS-DATEW       TO   SYU-F98
         MOVE    SYS-TIME(1:6)   TO   SYU-F99
         WRITE   SYU-REC
         ADD     1               TO   CRT-CNT
     ELSE
         ADD     IDT-F15         TO   SYU-F09
         MOVE    SYS-DATEW       TO   SYU-F98
         MOVE    SYS-TIME(1:6)   TO   SYU-F99
         REWRITE SYU-REC
         ADD     1               TO   RWT-CNT
     END-IF.
*
 200-MAIN-010.
     PERFORM BUMIDTF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    部門間移動抽出ファイル読込
************************************************************
 BUMIDTF-READ-SEC                   SECTION.
*
     READ   BUMIDTF
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        BUMIDTF-READ-EXIT
     END-READ.
*件数カウント
     ADD     1         TO        IN-CNT.
*
 BUMIDTF-READ-EXIT.
     EXIT.
************************************************************
*    部門間移動集計ファイル読込
************************************************************
 BUMSYUF-READ-SEC                     SECTION.
*索引キーセット
*振替先部門セット
     IF  IDT-F02  =  40
         IF  IDT-F05  = 0  OR  2  OR  4  OR  6
             MOVE   IDT-F01      TO   SYU-F01  WK-SYU-F01
         ELSE
             MOVE   IDT-F43      TO   SYU-F01  WK-SYU-F01
         END-IF
     END-IF.
*
     IF  IDT-F02  =  41
         IF  IDT-F05  = 0  OR  2  OR  4  OR  6
             MOVE   IDT-F43      TO   SYU-F01  WK-SYU-F01
         ELSE
             MOVE   IDT-F01      TO   SYU-F01  WK-SYU-F01
         END-IF
     END-IF.
*振替倉庫
     MOVE    IDT-F18             TO   SYU-F02  WK-SYU-F02.
*振替元倉庫
     IF  IDT-F02  =  40
         IF  IDT-F05  = 0  OR  2  OR  4  OR  6
             MOVE   IDT-F43      TO   SYU-F03  WK-SYU-F03
         ELSE
             MOVE   IDT-F01      TO   SYU-F03  WK-SYU-F03
         END-IF
     END-IF.
*
     IF  IDT-F02  =  41
         IF  IDT-F05  = 0  OR  2  OR  4  OR  6
             MOVE   IDT-F01      TO   SYU-F03  WK-SYU-F03
         ELSE
             MOVE   IDT-F43      TO   SYU-F03  WK-SYU-F03
         END-IF
     END-IF.
*振替日
     MOVE    IDT-F10             TO   SYU-F04  WK-SYU-F04.
*サカタ商品ＣＤ
     MOVE    IDT-F11             TO   SYU-F05  WK-SYU-F05.
*品単
     MOVE    IDT-F12(1:5)        TO   SYU-F06  WK-SYU-F06.
     MOVE    IDT-F12(6:2)        TO   SYU-F07  WK-SYU-F07.
     MOVE    IDT-F12(8:1)        TO   SYU-F08  WK-SYU-F08.
     READ   BUMSYUF
            INVALID
            MOVE      "INV"      TO   BUMSYUF-INV-FLG
            NOT  INVALID
            MOVE      SPACE      TO   BUMSYUF-INV-FLG
     END-READ.
*
 BUMSYUF-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             BUMIDTF  BUMSYUF.
*
     DISPLAY "* BUMIDTF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* BUMIDTF (CRT   ) = " CRT-CNT  " *"  UPON CONS.
     DISPLAY "* BUMIDTF (RWT   ) = " RWT-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
