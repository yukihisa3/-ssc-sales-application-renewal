****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　ＡＣＯＳ計上エラーデータ削除処理  *
*    作成日／更新日　　　：　2018/01/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＡＣＯＳ計上エラーデータを読み、　*
*                        ：　部門間移動抽出Ｆを読み、存在した  *
*                        ：　場合、レコード削除を行なう。　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBZ0130B.
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
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   IDT-F06 IDT-F03
                                                 IDT-F05 IDT-F02
                                                 IDT-F07 IDT-F10
                                                 IDT-F04
                        FILE      STATUS    IS   IDT-STATUS.
****<< 日次売上エラーファイル >>******************************
     SELECT   TOKUERR   ASSIGN    TO        DA-01-S-TOKUERR
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TWK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 部門間移動抽出ファイル >>******************************
 FD  BUMIDTF.
     COPY     BUMIDTF   OF        XFDLIB
              JOINING   IDT       PREFIX.
****<< 日次売上エラーファイル >>******************************
 FD  TOKUERR.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TWK       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 IDT-STATUS           PIC  X(02).
     02 TWK-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBZ0130B".
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
 01  BUMIDTF-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  SKIP-CNT            PIC  9(07)   VALUE  0.
     03  DEL-CNT             PIC  9(07)   VALUE  0.
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
                   PROCEDURE  TOKUERR.
     MOVE   "TOKUERR"         TO    ERR-FL-ID.
     MOVE    TWK-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     TOKUERR.
     OPEN         I-O       BUMIDTF.
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
     PERFORM  TOKUERR-READ-SEC.
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
*部門間移動抽出Ｆ索引
     MOVE  TWK-F06        TO   IDT-F06.
     MOVE  TWK-F03        TO   IDT-F03.
     MOVE  TWK-F05        TO   IDT-F05.
     MOVE  TWK-F02        TO   IDT-F02.
     MOVE  TWK-F07        TO   IDT-F07.
     MOVE  TWK-F10        TO   IDT-F10.
     MOVE  TWK-F04        TO   IDT-F04.
     READ  BUMIDTF
           INVALID      MOVE  "INV"   TO  BUMIDTF-INV-FLG
           NOT INVALID  MOVE  SPACE   TO  BUMIDTF-INV-FLG
     END-READ.
*
     IF    BUMIDTF-INV-FLG = "INV"
           ADD    1            TO   SKIP-CNT
     ELSE
           DELETE  BUMIDTF
           ADD    1            TO   DEL-CNT
     END-IF.
*
     PERFORM TOKUERR-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 TOKUERR-READ-SEC                   SECTION.
     READ   TOKUERR
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        TOKUERR-READ-EXIT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF   TWK-F02  =  "40"  OR  "41"
          CONTINUE
     ELSE
          GO           TO        TOKUERR-READ-SEC
     END-IF.
*
 TOKUERR-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             BUMIDTF  TOKUERR.
*
     DISPLAY "* TOKUERR (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* BUMIDTF (SKIP  )= " SKIP-CNT " *"  UPON CONS.
     DISPLAY "* BUMIDTF (DELETE)= " DEL-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
