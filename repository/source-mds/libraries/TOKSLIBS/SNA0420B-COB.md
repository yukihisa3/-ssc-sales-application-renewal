# SNA0420B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0420B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＧ部苗業務システム連携　　　　　*
*    モジュール名　　　　：　商品名称マスタ一覧表出力　        *
*    作成日／更新日　　　：　12/08/15                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受取った取引先ＣＤの商品名称マスタ*
*                            一覧データを出力する。連携対象のみ*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNA0420B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 商品変換テーブルＣＳＶ >>******************************
     SELECT   MEISHOWK    ASSIGN  TO   DA-01-VI-MEISHOW1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  MWK-F01
                                                 MWK-F02
                                                 MWK-F03
                                                 MWK-F04
                        FILE    STATUS       IS  MWK-STATUS.
****<< 商品名称マスタ　　   >>******************************
     SELECT   HMEIMS    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE STATUS          IS  MEI-STATUS.
****<< 仕入先マスタ　　　　 >>******************************
     SELECT   ZSHIMS    ASSIGN  TO   DA-01-VI-ZSHIMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  SHI-F01
                        FILE STATUS          IS  SHI-STATUS.
****<< 条件ファイル　　　　 >>******************************
     SELECT   HJYOKEN   ASSIGN  TO   DA-01-VI-JYOKEN1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  JYO-F01
                                                 JYO-F02
                        FILE STATUS          IS  JYO-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 商品変換テーブルＣＳＶ >>******************************
 FD    MEISHOWK.
       COPY     MEISHOWK    OF        XFDLIB
                JOINING   MWK       PREFIX.
****<< 商品名称マスタ　　　　 >>******************************
 FD    HMEIMS.
       COPY     HMEIMS    OF        XFDLIB
                JOINING   MEI       PREFIX.
****<< 仕入先マスタ　　　　　 >>******************************
 FD    ZSHIMS.
       COPY     ZSHIMS    OF        XFDLIB
                JOINING   SHI       PREFIX.
****<< 条件ファイル　　　　　 >>******************************
 FD    HJYOKEN.
       COPY     HJYOKEN   OF        XFDLIB
                JOINING   JYO       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 MWK-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 SHI-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
****  カウンタ                ****
 01  HMEIMS-CNT              PIC  9(8)   VALUE  ZERO.
 01  MEISHOWK-CNT            PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  RENKEI-FLG              PIC  X(01)  VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  ZSHIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  MEISHOWK-INV-FLG        PIC  X(03)  VALUE  SPACE.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SNA0420B".
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
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MEISHOWK.
     MOVE   "MEISHOW1"        TO    ERR-FL-ID.
     MOVE    MWK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "MEIMS1  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSHIMS.
     MOVE   "ZSHIMS1  "        TO    ERR-FL-ID.
     MOVE    SHI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
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
 SNA0420B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SNA0420B-END.
     EXIT.
************************************************************
*      １．０   初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
     OPEN     INPUT     HMEIMS  ZSHIMS  HJYOKEN.
     OPEN     I-O       MEISHOWK.
*商品変換テーブルスタート
     PERFORM  HMEIMS-READ-SEC.
     IF  END-FLG  =  "END"
         DISPLAY NC"＃＃対象データ無＃＃" UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
************************************************************
*      ＡＬＬ　商品名称マスタ読込                          *
************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     READ     HMEIMS
              AT END    MOVE  "END"  TO  END-FLG
                        GO           TO  HMEIMS-READ-EXIT
     END-READ.
*
 MEI001.
     ADD      1              TO     HMEIMS-CNT.
     IF   HMEIMS-CNT(6:3)  =  "000" OR "500"
          DISPLAY "HMEIMS READ CNT = " HMEIMS-CNT UPON CONS
     END-IF.
 MEI002.
*小売連携対象区分チェック
     IF   MEI-F10  NOT =  "1"
          GO                 TO     HMEIMS-READ-SEC
     END-IF.
*商品名称マスタ一覧データ存在チェック
     PERFORM MEISHOWK-READ-SEC.
     IF  MEISHOWK-INV-FLG = SPACE
          GO                 TO     HMEIMS-READ-SEC
     END-IF.
*
 HMEIMS-READ-EXIT.
     EXIT.
************************************************************
*      ＡＬＬ　　仕入先マスタ読込　　　　                  *
************************************************************
 ZSHIMS-READ-SEC        SECTION.
*
     MOVE     MEI-F05      TO     SHI-F01.
     READ     ZSHIMS
              INVALID       MOVE  "INV"   TO  ZSHIMS-INV-FLG
              NOT  INVALID  MOVE  SPACE   TO  ZSHIMS-INV-FLG
     END-READ.
*
 ZSHIMS-READ-EXIT.
     EXIT.
************************************************************
*      ＡＬＬ　　条件ファイル読込　　　　                  *
************************************************************
 HJYOKEN-READ-SEC       SECTION.
*
     READ     HJYOKEN
              INVALID       MOVE  "INV"   TO  HJYOKEN-INV-FLG
              NOT  INVALID  MOVE  SPACE   TO  HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
************************************************************
*      ＡＬＬ　　商品名称マスタ一覧ワーク読込　            *
************************************************************
 MEISHOWK-READ-SEC      SECTION.
*
     MOVE     MEI-F011     TO     MWK-F01.
     MOVE     MEI-F0121    TO     MWK-F02.
     MOVE     MEI-F0122    TO     MWK-F03.
     MOVE     MEI-F0123    TO     MWK-F04.
     READ     MEISHOWK
              INVALID       MOVE  "INV"   TO  MEISHOWK-INV-FLG
              NOT  INVALID  MOVE  SPACE   TO  MEISHOWK-INV-FLG
     END-READ.
*
 MEISHOWK-READ-EXIT.
     EXIT.
************************************************************
*      ２．０    メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*商品名称マスタ一覧ワーク初期化
     MOVE     SPACE         TO      MWK-REC.
     INITIALIZE                     MWK-REC.
*項目転送
*商品ＣＤ／品単ＣＤ
     MOVE     MEI-F011      TO      MWK-F01.
     MOVE     MEI-F0121     TO      MWK-F02.
     MOVE     MEI-F0122     TO      MWK-F03.
     MOVE     MEI-F0123     TO      MWK-F04.
*商品名／カナ
     MOVE     MEI-F021      TO      MWK-F05.
     MOVE     MEI-F022      TO      MWK-F06.
     MOVE     MEI-F031      TO      MWK-F07.
     MOVE     MEI-F032      TO      MWK-F08.
*単価関係
     MOVE     MEI-F041      TO      MWK-F09.
     MOVE     MEI-F042      TO      MWK-F10.
     MOVE     MEI-F043      TO      MWK-F11.
*仕入先ＣＤ／仕入先名
     MOVE     MEI-F05       TO      MWK-F12.
     PERFORM  ZSHIMS-READ-SEC.
     IF  ZSHIMS-INV-FLG  =  SPACE
         MOVE SHI-F02       TO      MWK-F13
     ELSE
         MOVE ALL NC"＊"    TO      MWK-F13
     END-IF.
*ＪＡＮＣＤ
     MOVE     MEI-F06       TO      MWK-F14.
*入数
     MOVE     MEI-F07       TO      MWK-F15.
*２０分類
     MOVE     "10"          TO      JYO-F01.
     MOVE     MEI-F09       TO      JYO-F02  MWK-F16.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = SPACE
         MOVE JYO-F03       TO      MWK-F17
     ELSE
         MOVE ALL NC"＊"    TO      MWK-F17
     END-IF.
*分類
     MOVE     "91"          TO      JYO-F01.
     MOVE     MEI-F90       TO      JYO-F02  MWK-F18.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = SPACE
         MOVE JYO-F03       TO      MWK-F19
     ELSE
         MOVE ALL NC"＊"    TO      MWK-F19
     END-IF.
*小売連携対象区分
     MOVE     MEI-F10       TO      MWK-F20.
*出力
     WRITE    MWK-REC.
     ADD      1             TO      MEISHOWK-CNT.
*商品変換ＴＢＬ読込
     PERFORM  HMEIMS-READ-SEC.
*
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*
     CLOSE    MEISHOWK  HMEIMS  ZSHIMS  HJYOKEN.

     IF    HMEIMS-CNT NOT = ZERO
     DISPLAY  " HMEIMS-READ-CNT    = " HMEIMS-CNT   UPON CONS
     DISPLAY  " MEISHOWK-WRITE-CNT = " MEISHOWK-CNT UPON CONS
     END-IF.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
