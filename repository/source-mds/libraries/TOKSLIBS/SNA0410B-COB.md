# SNA0410B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0410B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＧ部苗業務システム連携　　　　　*
*    モジュール名　　　　：　商品変換ＴＢＬ一覧表出力　        *
*    作成日／更新日　　　：　12/08/15                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受取った取引先ＣＤの商品変換ＴＢＬ*
*                            一覧データを出力する。連携対象のみ*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNA0410B.
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
     SELECT   SHOTBLWK    ASSIGN  TO   DA-01-VI-SHOTBLW1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  TWK-F01
                                                 TWK-F03
                        FILE    STATUS       IS  TWK-STATUS.
****<< 商品変換テーブル　   >>******************************
     SELECT   HSHOTBL    ASSIGN  TO   DA-01-VI-SHOTBL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  TBL-F01
                                                 TBL-F02
                        FILE STATUS          IS  TBL-STATUS.
****<< 商品名称マスタ　　   >>******************************
     SELECT   HMEIMS    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE STATUS          IS  MEI-STATUS.
****<< 取引先マスタ　　　　 >>******************************
     SELECT   HTOKMS    ASSIGN  TO   DA-01-VI-TOKMS2
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  TOK-F01
                        FILE STATUS          IS  TOK-STATUS.
****<< 倉庫マスタ　　　　　 >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  SOK-F01
                        FILE STATUS          IS  SOK-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 商品変換テーブルＣＳＶ >>******************************
 FD    SHOTBLWK.
       COPY     SHOTBLWK    OF        XFDLIB
                JOINING   TWK       PREFIX.
****<< 商品変換テーブル　　　 >>******************************
 FD    HSHOTBL.
       COPY     HSHOTBL    OF        XFDLIB
                JOINING   TBL       PREFIX.
****<< 商品名称マスタ　　　　 >>******************************
 FD    HMEIMS.
       COPY     HMEIMS    OF        XFDLIB
                JOINING   MEI       PREFIX.
****<< 取引先マスタ　　　　　 >>******************************
 FD    HTOKMS.
       COPY     HTOKMS    OF        XFDLIB
                JOINING   TOK       PREFIX.
****<< 倉庫マスタ　　　　　　 >>******************************
 FD    ZSOKMS.
       COPY     ZSOKMS    OF        XFDLIB
                JOINING   SOK       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TWK-STATUS           PIC  X(2).
     02 TBL-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 TOK-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
****  カウンタ                ****
 01  HSHOTBL-CNT             PIC  9(8)   VALUE  ZERO.
 01  SHOTBLWK-CNT            PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  RENKEI-FLG              PIC  X(01)  VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  ZSOKMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  SHOTBLWK-INV-FLG        PIC  X(03)  VALUE  SPACE.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SNA0410B".
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
 LINKAGE               SECTION.
 01  PARA-TOKCD            PIC 9(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION  USING  PARA-TOKCD.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SHOTBLWK.
     MOVE   "SHOTBLW1  "        TO    ERR-FL-ID.
     MOVE    TWK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTOKMS.
     MOVE   "TOKMS2  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HSHOTBL.
     MOVE   "HSHOTBL  "        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS1 "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SNA0410B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SNA0410B-END.
     EXIT.
************************************************************
*      １．０   初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
     OPEN     INPUT     HSHOTBL  HMEIMS  HTOKMS  ZSOKMS.
     OPEN     I-O       SHOTBLWK.
*商品変換テーブルスタート
     MOVE     SPACE              TO      TBL-REC.
     INITIALIZE                          TBL-REC.
*
     MOVE     PARA-TOKCD         TO      TBL-F01.
     START    HSHOTBL    KEY  IS  >=  TBL-F01  TBL-F02
              INVALID   MOVE  "END"  TO  END-FLG
                      DISPLAY NC"＃＃対象データ無＃＃" UPON CONS
                        GO   TO   INIT-END
     END-START.
*
     PERFORM  HSHOTBL-READ-SEC.
     IF  END-FLG  =  "END"
         DISPLAY NC"＃＃対象データ無＃＃" UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
************************************************************
*      ＡＬＬ　商品変換ＴＢＬ読込                          *
************************************************************
 HSHOTBL-READ-SEC       SECTION.
*
     READ     HSHOTBL
              NEXT AT END    MOVE  "END"  TO  END-FLG
                             GO           TO  HSHOTBL-READ-EXIT
     END-READ.
*
 TBL001.
     ADD      1              TO     HSHOTBL-CNT.
     IF   HSHOTBL-CNT(6:3)  =  "000" OR "500"
          DISPLAY "HSHOTBL READ CNT = " HSHOTBL-CNT UPON CONS
     END-IF.
 TBL002.
*取引先コードチェック
     IF   TBL-F01  >  PARA-TOKCD
          MOVE  "END"        TO     END-FLG
          GO                 TO     HSHOTBL-READ-EXIT
     END-IF.
 TBL003.
*小売連携区分チェック
     PERFORM  HMEIMS-READ-SEC.
*小売連携対象でない商品の場合は、次レコード読込
     IF   RENKEI-FLG NOT = "1"
          GO                 TO     HSHOTBL-READ-SEC
     END-IF.
 TBL004.
*商品変換テーブル一覧ワーク存在チェック
*存在の場合は、次レコード処理へ
     PERFORM  SHOTBLWK-READ-SEC.
     IF  SHOTBLWK-INV-FLG = SPACE
          GO                 TO     HSHOTBL-READ-SEC
     END-IF.
*
 HSHOTBL-READ-EXIT.
     EXIT.
************************************************************
*      ＡＬＬ　　商品名称マスタ読込　　　                  *
************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     MOVE     TBL-F031     TO     MEI-F011.
     MOVE     TBL-F0321    TO     MEI-F0121.
     MOVE     TBL-F0322    TO     MEI-F0122.
     MOVE     TBL-F0323    TO     MEI-F0123.
     READ     HMEIMS
              INVALID       MOVE  "INV"   TO  HMEIMS-INV-FLG
                            MOVE  SPACE   TO  RENKEI-FLG
                            GO            TO  HMEIMS-READ-SEC
              NOT  INVALID  MOVE  SPACE   TO  HMEIMS-INV-FLG
     END-READ.
*
     IF   MEI-F10  =  "1"
          MOVE   "1"        TO    RENKEI-FLG
     ELSE
          MOVE   SPACE      TO    RENKEI-FLG
     END-IF.
*
 HMEIMS-READ-EXIT.
     EXIT.
************************************************************
*      ＡＬＬ　　取引先マスタ　　　　　　                  *
************************************************************
 HTOKMS-READ-SEC        SECTION.
*
     MOVE     TBL-F01      TO     TOK-F01.
     READ     HTOKMS
              INVALID       MOVE  "INV"   TO  HTOKMS-INV-FLG
              NOT  INVALID  MOVE  SPACE   TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
************************************************************
*      ＡＬＬ　　倉庫マスタ　　　　　　　                  *
************************************************************
 ZSOKMS-READ-SEC        SECTION.
*
     MOVE     TBL-F04      TO     SOK-F01.
     READ     ZSOKMS
              INVALID       MOVE  "INV"   TO  ZSOKMS-INV-FLG
              NOT  INVALID  MOVE  SPACE   TO  ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
************************************************************
*      ＡＬＬ　　商品変換テーブル一覧ワーク読込            *
************************************************************
 SHOTBLWK-READ-SEC      SECTION.
*
     MOVE     TBL-F01      TO     TWK-F01.
     MOVE     TBL-F02      TO     TWK-F03.
     READ     SHOTBLWK
              INVALID       MOVE  "INV"   TO  SHOTBLWK-INV-FLG
              NOT  INVALID  MOVE  SPACE   TO  SHOTBLWK-INV-FLG
     END-READ.
*
 SHOTBLWK-READ-EXIT.
     EXIT.
************************************************************
*      ２．０    メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*商品変換テーブル一覧ワーク初期化
     MOVE     SPACE         TO      TWK-REC.
     INITIALIZE                     TWK-REC.
*項目転送
*取引先ＣＤ
     MOVE     TBL-F01       TO      TWK-F01.
*取引先名
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  =  SPACE
         MOVE TOK-F02       TO      TWK-F02
     ELSE
         MOVE ALL NC"＊"    TO      TWK-F02
     END-IF.
*量販店商品ＣＤ
     MOVE     TBL-F02       TO      TWK-F03.
*サカタ商品ＣＤ
     MOVE     TBL-F031      TO      TWK-F04.
     MOVE     TBL-F0321     TO      TWK-F05.
     MOVE     TBL-F0322     TO      TWK-F06.
     MOVE     TBL-F0323     TO      TWK-F07.
*商品名１、２
     MOVE     MEI-F021      TO      TWK-F08.
     MOVE     MEI-F022      TO      TWK-F09.
*出荷場所
     MOVE     TBL-F04       TO      TWK-F10.
     PERFORM  ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG  =  SPACE
         MOVE SOK-F02       TO      TWK-F11
     ELSE
         MOVE ALL NC"＊"    TO      TWK-F11
     END-IF.
*単価関係
     MOVE     TBL-F09       TO      TWK-F12.
     MOVE     TBL-F05       TO      TWK-F13.
     MOVE     TBL-F06       TO      TWK-F14.
*分類
     MOVE     TBL-F07       TO      TWK-F15
*_番
     MOVE     TBL-F081      TO      TWK-F16
     MOVE     TBL-F082      TO      TWK-F17
     MOVE     TBL-F083      TO      TWK-F18
*出力
     WRITE    TWK-REC.
     ADD      1             TO      SHOTBLWK-CNT.
*商品変換ＴＢＬ読込
     PERFORM  HSHOTBL-READ-SEC.
*
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*
     CLOSE    SHOTBLWK  HSHOTBL  HMEIMS  HTOKMS  ZSOKMS.

     IF    HSHOTBL-CNT NOT = ZERO
     DISPLAY  " HSHOTBL-READ-CNT   = " HSHOTBL-CNT  UPON CONS
     DISPLAY  " SHOTBLWK-WRITE-CNT = " SHOTBLWK-CNT UPON CONS
     END-IF.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
