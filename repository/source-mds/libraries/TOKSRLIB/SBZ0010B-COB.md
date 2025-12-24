# SBZ0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBZ0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　部門間在庫移動ＤＴ抽出　　　　　  *
*    作成日／更新日　　　：　2018/01/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　計上用売上データＷＫを読み、伝区　*
*                        ：　＝４０、４１で部門間移動マスタに  *
*                        ：　登録されたデータの抽出を行なう。  *
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/04/18 高橋　　　数量＝０データは対象としない
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBZ0010B.
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
****<< 計上用売上データＷＫ >>********************************
     SELECT   ACOSDATA  ASSIGN    TO        DA-01-S-ACOSDATA
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   ACS-STATUS.
****<< 部門間移動抽出ファイル >>******************************
     SELECT   BUMIDTF   ASSIGN    TO        DA-01-VI-BUMIDTL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   IDT-F06 IDT-F03
                                                 IDT-F05 IDT-F02
                                                 IDT-F07 IDT-F10
                                                 IDT-F04
                        FILE      STATUS    IS   IDT-STATUS.
****<< 部門間移動マスタ >>************************************
     SELECT   BUMIDOF   ASSIGN    TO        DA-01-VI-BUMIDOL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   IDO-F01 IDO-F02
                                                 IDO-F03
                        FILE      STATUS    IS   IDO-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 計上用売上データＷＫ >>********************************
 FD  ACOSDATA.
     COPY     ACOSDATA  OF        XFDLIB
              JOINING   ACS       PREFIX.
****<< 部門間移動抽出ファイル >>******************************
 FD  BUMIDTF.
     COPY     BUMIDTF   OF        XFDLIB
              JOINING   IDT       PREFIX.
****<< 部門間移動マスタ >>************************************
 FD  BUMIDOF.
     COPY     BUMIDOF   OF        XFDLIB
              JOINING   IDO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 IDT-STATUS           PIC  X(02).
     02 IDO-STATUS           PIC  X(02).
     02 ACS-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBZ0010B".
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
 01  BUMIDOF-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  SKIP-CNT            PIC  9(07)   VALUE  0.
     03  CRT-CNT             PIC  9(07)   VALUE  0.
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
                   PROCEDURE  BUMIDOF.
     MOVE   "BUMIDOL1"        TO    ERR-FL-ID.
     MOVE    IDT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ACOSDATA.
     MOVE   "ACOSDATA"        TO    ERR-FL-ID.
     MOVE    ACS-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     ACOSDATA  BUMIDOF.
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
     PERFORM  ACOSDATA-READ-SEC.
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
     MOVE  ACS-F06        TO   IDT-F06.
     MOVE  ACS-F03        TO   IDT-F03.
     MOVE  ACS-F05        TO   IDT-F05.
     MOVE  ACS-F02        TO   IDT-F02.
     MOVE  ACS-F07        TO   IDT-F07.
     MOVE  ACS-F10        TO   IDT-F10.
     MOVE  ACS-F04        TO   IDT-F04.
     READ  BUMIDTF
           INVALID      MOVE  "INV"   TO  BUMIDTF-INV-FLG
           NOT INVALID  MOVE  SPACE   TO  BUMIDTF-INV-FLG
     END-READ.
*
     IF    BUMIDTF-INV-FLG = SPACE
           ADD    1                  TO   SKIP-CNT
           GO                        TO   200-MAIN-010
     END-IF.
*部門間移動抽出Ｆ初期化
     MOVE         SPACE              TO   IDT-REC.
     INITIALIZE                           IDT-REC.
*　レコードセット
     MOVE         ACS-REC            TO   IDT-REC.
*　振替先部門セット
     MOVE         IDO-F04            TO   IDT-F43
     WRITE  IDT-REC.
     ADD          1                  TO   CRT-CNT.
*
 200-MAIN-010.
     PERFORM ACOSDATA-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    計上用日次データ読込
************************************************************
 ACOSDATA-READ-SEC                   SECTION.
*
     READ   ACOSDATA
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        ACOSDATA-READ-EXIT
     END-READ.
*件数カウント
     ADD     1         TO        IN-CNT.
*伝票区分＝４０、４１以外は読み飛ばし
     IF   ACS-F02  =  "40"  OR  "41"
          CONTINUE
     ELSE
          ADD    1     TO        SKIP-CNT
          GO           TO        ACOSDATA-READ-SEC
     END-IF.
*#2018/04/18 NAV ST 数量＝０は対象としない
     IF   ACS-F15  =  ZERO
          ADD    1     TO        SKIP-CNT
          GO           TO        ACOSDATA-READ-SEC
     END-IF.
*#2018/04/18 NAV ED
*部門間移動マスタ存在チェック（存在しない場合は読み飛ばし）
     PERFORM  BUMIDOF-READ-SEC.
*
     IF  BUMIDOF-INV-FLG = "INV"
          ADD    1     TO        SKIP-CNT
          GO           TO        ACOSDATA-READ-SEC
     END-IF.
*
 ACOSDATA-READ-EXIT.
     EXIT.
************************************************************
*    部門間移動マスタ読込
************************************************************
 BUMIDOF-READ-SEC                     SECTION.
*
     MOVE   ACS-F01              TO   IDO-F01.
     MOVE   ACS-F39              TO   IDO-F02.
     MOVE   ACS-F18              TO   IDO-F03.
     READ   BUMIDOF
            INVALID
            MOVE      "INV"      TO   BUMIDOF-INV-FLG
            NOT  INVALID
            MOVE      SPACE      TO   BUMIDOF-INV-FLG
     END-READ.
*
 BUMIDOF-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             ACOSDATA   BUMIDTF  BUMIDOF.
*
     DISPLAY "* ACOSDATA (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* BUMIDTF (SKIP  ) = " SKIP-CNT " *"  UPON CONS.
     DISPLAY "* BUMIDTF (WRITE ) = " CRT-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
