****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫管理表　　　　　              *
*    作成日／更新日　　　：　00/05/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　抽出済在庫マスタから、在庫管理表　*
*                            を印刷する（月末在庫）　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0106L.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   YA
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 抽出済在庫マスタ >>**********************************
     SELECT   ZAIWK     ASSIGN    TO        DA-01-S-ZZAIWK
                        FILE      STATUS    IS   ZWK-STATUS.
****<< 商品名称マスタ　 >>******************************
     SELECT   MEIM      ASSIGN    TO        DA-01-VI-HMEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
****<< 倉庫マスタ　 >>**********************************
     SELECT   SOKM      ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
****<< 条件ファイル >>**********************************
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRTF      ASSIGN  TO   LP-04.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 抽出済在庫マスタ >>*********************************
 FD  ZAIWK
              BLOCK CONTAINS  4   RECORDS.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZWK       PREFIX.
****<< 商品名称マスタ　>>**********************************
 FD  MEIM.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****<< 倉庫マスタ >>*************************************
 FD  SOKM.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
****<< 条件ファイル >>*************************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRTF.
 01    PRT-REC               PIC  X(200).
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZWK-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  LINE-CNT            PIC  9(02)    VALUE     99.
     03  PG-CNT              PIC  9(07)    VALUE     ZERO.
     03  IN-CNT              PIC  9(07)    VALUE     ZERO.
     03  OUT-CNT             PIC  9(07)    VALUE     ZERO.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(8).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  9(04).
       03  SYS-MM            PIC  9(02).
       03  SYS-DD            PIC  9(02).
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER              PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN          PIC  N(06)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"）".
*
 01  KEY-AREA.
     03  NEW-KEY             PIC  X(02).
     03  OLD-KEY             PIC  X(02).
*       帳票編集エリア
 01  HEAD1.
     03  FILLER                  PIC  X(01)     VALUE  SPACE.
     03  FILLER                  PIC  X(08)     VALUE "SZA0106L".
     03  FILLER                  PIC  X(08)     VALUE  SPACE.
     03  FILLER                  PIC  N(21)     VALUE
         NC"＊＊＊　在　庫　管　理　表（前月末）＊＊＊"
                            CHARACTER TYPE IS   YB-21.
     03  HD1-TOKHAN              PIC  N(08)     VALUE  SPACE
                            CHARACTER TYPE IS   YA.
     03  FILLER                  PIC  X(05)     VALUE  SPACE.
     03  FILLER                  PIC  N(03)     VALUE
         NC"処理日"         CHARACTER TYPE IS   YA.
     03  FILLER                  PIC  X(01)     VALUE  ":".
     03  HD1-YY                  PIC  9(04)..
     03  HD1-MM                  PIC  Z9..
     03  HD1-DD                  PIC  Z9.
     03  FILLER                  PIC  X(03)     VALUE  SPACE.
     03  FILLER                  PIC  N(01)     VALUE  NC"頁"
                            CHARACTER TYPE IS   YA.
     03  FILLER                  PIC  X(01)     VALUE  ":".
     03  HD1-PG                  PIC  ZZZ9.
*
 01  HEAD2                  CHARACTER TYPE IS   YA.
     03  FILLER                  PIC  X(03)     VALUE  SPACE.
     03  FILLER                  PIC  N(03)     VALUE
         NC"倉庫：".
     03  HD2-CD                  PIC  X(02).
     03  FILLER                  PIC  X(01)     VALUE  SPACE.
     03  HD2-NM                  PIC  N(18).
*
 01  HEAD3                  CHARACTER TYPE IS   YA.
     03  FILLER                  PIC  X(01)     VALUE  SPACE.
     03  FILLER                  PIC  N(02)     VALUE  NC"商品".
     03  FILLER                  PIC  X(06)     VALUE  "ｺｰﾄﾞ".
     03  FILLER                  PIC  N(03)     VALUE
         NC"品　単".
     03  FILLER                  PIC  X(03)     VALUE  SPACE.
     03  FILLER                  PIC  N(03)     VALUE
         NC"棚　番".
     03  FILLER                  PIC  X(02)     VALUE  SPACE.
     03  FILLER                  PIC  N(05)     VALUE
         NC"商　品　名".
     03  FILLER                  PIC  X(27)     VALUE  SPACE.
     03  FILLER                  PIC  N(06)     VALUE
         NC"前月末在庫数".
     03  FILLER                  PIC  X(04)     VALUE  SPACE.
     03  FILLER                  PIC  N(05)     VALUE
         NC"当月入庫数".
     03  FILLER                  PIC  X(04)     VALUE  SPACE.
     03  FILLER                  PIC  N(05)     VALUE
         NC"当月出庫数".
     03  FILLER                  PIC  X(04)     VALUE  SPACE.
*****03  FILLER                  PIC  N(05)     VALUE
*****    NC"当月返品数".
*****03  FILLER                  PIC  X(02)     VALUE  SPACE.
     03  FILLER                  PIC  N(06)     VALUE
         NC"当月末在庫数".
*
 01  HEAD4                  CHARACTER TYPE IS   YA.
     03  FILLER                  PIC  X(01)     VALUE  SPACE.
     03  FILLER                  PIC  N(67) VALUE ALL NC"─".
*
 01  BODY1.
     03  FILLER                  PIC  X(01)     VALUE  SPACE.
     03  BD-CD                   PIC  X(08).
     03  FILLER                  PIC  X(01)     VALUE  SPACE.
     03  BD-HIN                  PIC  X(08).
     03  FILLER                  PIC  X(01)     VALUE  SPACE.
     03  SB-TANA.
         05  BD-TANA1            PIC  X(01).
         05  BD-TANAA            PIC  X(01).
         05  BD-TANA2            PIC  X(03).
         05  BD-TANAB            PIC  X(01).
         05  BD-TANA3            PIC  X(02).
     03  FILLER                  PIC  X(01)     VALUE  SPACE.
     03  BD-SNM1                 PIC  N(15) CHARACTER TYPE IS YB.
     03  BD-SNM2                 PIC  N(09) CHARACTER TYPE IS YB.
     03  BD-ZZAISU               PIC  ---,---,--9.99.
     03  BD-TNYUSU               PIC  ---,---,--9.99.
     03  BD-TSHUSU               PIC  ---,---,--9.99.
*****03  BD-THEPSU               PIC  ---,---,--9.99.
     03  BD-TZAISU               PIC  ---,---,--9.99.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0106L".
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
                   PROCEDURE  ZAIWK.
     MOVE   "ZZAIWK  "        TO    ERR-FL-ID.
     MOVE    ZWK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MEIM.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SOKM.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
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
     OPEN         INPUT     ZAIWK.
     OPEN         INPUT     MEIM.
     OPEN         INPUT     SOKM.
     OPEN         INPUT     HJYOKEN.
     OPEN         OUTPUT    PRTF.
*       システム日付の取得
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
     MOVE      LINK-OUT-YMD       TO   SYSYMD.
     MOVE      SYS-YY             TO   HD1-YY.
     MOVE      SYS-MM             TO   HD1-MM.
     MOVE      SYS-DD             TO   HD1-DD.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03            TO   HEN-TOKHAN
     END-READ.
     MOVE     HEN-TOKHAN-AREA     TO   HD1-TOKHAN.
*     抽出済在庫マスタの読込処理
     PERFORM           ZAIWK-READ.
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
     MOVE    NEW-KEY             TO   OLD-KEY.
*
     PERFORM      PRT-OUT
       UNTIL      NEW-KEY   NOT  =    OLD-KEY.
*
     MOVE    66                  TO   LINE-CNT.
 200-MAIN-SEC-EXT.
     EXIT.
*----------------------------------------------------------*
*                帳票出力処理　
*----------------------------------------------------------*
 PRT-OUT                         SECTION.
     MOVE    SPACE               TO   BODY1.
* 商品コード
     MOVE    ZWK-F021            TO   BD-CD.
* 品単
     MOVE    ZWK-F022            TO   BD-HIN.
* 棚番
     IF      ZWK-F03    NOT =   SPACE
             MOVE    ZWK-F03(1:1)     TO   BD-TANA1
             MOVE    ZWK-F03(2:3)     TO   BD-TANA2
             MOVE    ZWK-F03(5:2)     TO   BD-TANA3
             MOVE    "-"              TO   BD-TANAA
                                           BD-TANAB
     END-IF.
* 商品名
     MOVE    ZWK-F021            TO   MEI-F011.
     MOVE    ZWK-F022            TO   MEI-F012.
     READ    MEIM
        INVALID   KEY
             CONTINUE
        NOT  INVALID   KEY
             MOVE   MEI-F021     TO   BD-SNM1
             MOVE   MEI-F022     TO   BD-SNM2
     END-READ.
* 前月末在庫数
     MOVE    ZWK-F17             TO   BD-ZZAISU.
* 当月入庫数
     MOVE    ZWK-F15             TO   BD-TNYUSU.
* 当月出庫数
     MOVE    ZWK-F16             TO   BD-TSHUSU.
* 当月返品数
*****MOVE    ZWK-F10             TO   BD-THEPSU.
* 当月末在庫数
     MOVE    ZWK-F05             TO   BD-TZAISU.
*
     IF      LINE-CNT  >=   62
             PERFORM        KAI-PAGE
     END-IF.
*
     WRITE   PRT-REC   FROM      BODY1     AFTER     1
     ADD     1                   TO   LINE-CNT.
*
     PERFORM           ZAIWK-READ.
 PRT-OUT-EXT.
     EXIT.
*----------------------------------------------------------*
*                改ページ処理
*----------------------------------------------------------*
 KAI-PAGE                        SECTION.
     IF      PG-CNT    >    0
             MOVE      SPACE          TO   PRT-REC
             WRITE     PRT-REC   AFTER     PAGE
     END-IF.
*
     ADD     1                   TO   PG-CNT.
     MOVE    PG-CNT              TO   HD1-PG.
     MOVE    OLD-KEY             TO   HD2-CD
                                      SOK-F01.
     READ    SOKM
        INVALID   KEY
             CONTINUE
        NOT  INVALID  KEY
             MOVE   SOK-F02      TO   HD2-NM
     END-READ.
*
     WRITE   PRT-REC   FROM      HEAD1     AFTER     2.
     WRITE   PRT-REC   FROM      HEAD2     AFTER     1.
     WRITE   PRT-REC   FROM      HEAD4     AFTER     1.
     WRITE   PRT-REC   FROM      HEAD3     AFTER     1.
     WRITE   PRT-REC   FROM      HEAD4     AFTER     1.
     MOVE    6                   TO   LINE-CNT.
 KAI-PAGE-EXT.
     EXIT.
************************************************************
*            抽出済在庫マスタの読込処理
************************************************************
 ZAIWK-READ                      SECTION.
     READ    ZAIWK
        AT   END
             MOVE      "END"     TO   END-FLG
             MOVE      HIGH-VALUE     TO   NEW-KEY
             GO        TO        ZAIWK-READ-EXT
     END-READ.
     ADD     1                   TO   IN-CNT.
*
*****IF      ZWK-F07   =    ZERO
*****   AND  ZWK-F08   =    ZERO
*****   AND  ZWK-F10   =    ZERO
*****   AND  ZWK-F04   =    ZERO
     IF      ZWK-F15   =    ZERO
        AND  ZWK-F16   =    ZERO
        AND  ZWK-F05   =    ZERO
        GO   TO             ZAIWK-READ
     END-IF.
*
     ADD     1                   TO   OUT-CNT.
     MOVE    ZWK-F01             TO   NEW-KEY.
 ZAIWK-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE    HJYOKEN  ZAIWK     SOKM      MEIM      PRTF.
*
     DISPLAY "* ZZAIWK  (INPUT) = " IN-CNT  " *" UPON CONS.
     DISPLAY "* ZZAIWK  (OUTPUT)= " OUT-CNT " *" UPON CONS.
     DISPLAY "* PRTF    (PAGE)  = " PG-CNT  " *" UPON CONS.
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
