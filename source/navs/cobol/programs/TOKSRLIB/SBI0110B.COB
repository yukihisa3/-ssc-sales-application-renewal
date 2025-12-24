****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＢＩツール連携　　　　　　　　　　*
*    モジュール名　　　　：　日次計上エラーデータチェック　　  *
*    作成日／更新日　　　：　2018/12/14                        *
*    作成者／更新者　　　：  NAV TAKAHASHI                     *
*    処理概要　　　　　　：　ＡＣＯＳ計上エラーデータを読み、　*
*                        ：　ＢＩ編集用ファイルを読み、存在し  *
*                        ：　た場合、エラー区分に１をセット　  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBI0110B.
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
****<< ＢＩ用編集ファイル　　 >>******************************
     SELECT   BITRNDTF  ASSIGN    TO        DA-01-VI-BITRNDT2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TRN-F04 TRN-F05
                                                 TRN-F06 TRN-F07
                                                 TRN-F08 TRN-F09
                                                 TRN-F10
                        FILE      STATUS    IS   TRN-STATUS.
****<< 日次売上エラーファイル >>******************************
     SELECT   TOKUERR   ASSIGN    TO        DA-01-S-TOKUERR
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TWK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< ＢＩ用編集ファイル　　 >>******************************
 FD  BITRNDTF.
     COPY     BITRNDTF   OF       XFDLIB
              JOINING   TRN       PREFIX.
****<< 日次売上エラーファイル >>******************************
 FD  TOKUERR.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TWK       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TRN-STATUS           PIC  X(02).
     02 TWK-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0110B".
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
 01  BITRNDTF-INV-FLG        PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  SKIP-CNT            PIC  9(07)   VALUE  0.
     03  CHK-CNT             PIC  9(07)   VALUE  0.
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
                   PROCEDURE  BITRNDTF.
     MOVE   "BUMIDTL1"        TO    ERR-FL-ID.
     MOVE    TRN-STATUS       TO    ERR-STCD.
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
     OPEN         I-O       BITRNDTF.
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
     MOVE  TWK-F06        TO   TRN-F04.
     MOVE  TWK-F03        TO   TRN-F05.
     MOVE  TWK-F05        TO   TRN-F06.
     MOVE  TWK-F02        TO   TRN-F07.
     MOVE  TWK-F07        TO   TRN-F08.
     MOVE  TWK-F10        TO   TRN-F09.
     MOVE  TWK-F04        TO   TRN-F10.
     READ  BITRNDTF
           INVALID      MOVE  "INV"   TO  BITRNDTF-INV-FLG
           NOT INVALID  MOVE  SPACE   TO  BITRNDTF-INV-FLG
     END-READ.
*
     IF    BITRNDTF-INV-FLG = "INV"
           ADD    1            TO   SKIP-CNT
     ELSE
           MOVE   "1"          TO   TRN-F11
           ADD    1            TO   CHK-CNT
           REWRITE  TRN-REC
     END-IF.
*
     PERFORM TOKUERR-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            ＡＣＯＳ計上エラーファイル読込
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
 TOKUERR-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             BITRNDTF  TOKUERR.
*
     DISPLAY "* TOKUERR  (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* BITRNDTF (SKIP  ) = " SKIP-CNT " *"  UPON CONS.
     DISPLAY "* BITRNDTF (ERROR ) = " CHK-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
