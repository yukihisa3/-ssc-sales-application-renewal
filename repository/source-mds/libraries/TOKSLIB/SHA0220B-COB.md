# SHA0220B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SHA0220B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　発注管理（自動）                  *
*    業務名　　　　　　　：　自動発注数計算                    *
*    モジュール名　　　　：　欠品チェック（自動発注）　　　　　*
*    作成日／更新日　　　：　2003/06/06                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　在庫ワークを読み、該当レコードの  *
*                            安全在庫数をチェックして安全在庫  *
*                            数以下の場合、自動発注ワーク作成。*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0220B.
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
*---<<  商品在庫マスタ　　      >>---*
     SELECT   INF      ASSIGN    TO        DA-01-S-INF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   IN-STATUS.
*
*---<<  条件ファイル　　　　　　>>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-HJYOKEN
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
*---<<  商品コード変換テーブル　>>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-HSHOTBL
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F04
                                                 SHO-F08
                                                 SHO-F031
                                                 SHO-F032
                        FILE      STATUS    IS   SHO-STATUS.
*
*---<<  商品名称マスタ　　　    >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-HMEIMS
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F012
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<< 自動発注ワーク  >>---*
     SELECT   AUTHACF   ASSIGN    TO        DA-01-VI-AUTHACF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   AUT-F01
                                                 AUT-F02
                                                 AUT-F03
                                                 AUT-F04
                        FILE      STATUS    IS   AUT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  商品在庫マスタ　　      >>---*
 FD  INF                BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     ZAMZAIF   OF     XFDLIB
              JOINING   IN        PREFIX.
*---<<  条件ファイル　　　　　　>>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  商品コード変換テーブル　>>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品名称マスタ　　　　　>>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  自動発注ワーク  >>---*
 FD  AUTHACF.
     COPY     AUTHACF   OF        XFDLIB
              JOINING   AUT       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  IN-STATUS           PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  SHO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  AUT-STATUS          PIC  X(02).
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG             PIC  X(03)     VALUE SPACE.
     02  WK-KEISU-FLG        PIC  9(01)     VALUE ZERO.
****  カウンタ ***
 01  CNT-AREA.
     02  READ-CNT            PIC  9(05)     VALUE ZERO.
     02  SYORI-CNT           PIC  9(05)     VALUE ZERO.
     02  SYOTB-CNT           PIC  9(05)     VALUE ZERO.
     02  MEI-CNT             PIC  9(05)     VALUE ZERO.
     02  JUMP-CNT            PIC  9(05)     VALUE ZERO.
     02  WRITE-CNT           PIC  9(05)     VALUE ZERO.
****  添字　****
 01  INDEX-AREA.
     02  IX                  PIC  9(02).
****  ワ－ク  ***
 01  WK-AREA.
     02  WK-ANZENSU          PIC S9(09)     VALUE  ZERO.
     02  WK-HACHSU           PIC S9(09)V99  VALUE  ZERO.
     02  WK-ZAIKOSU          PIC S9(09)V99  VALUE  ZERO.
     02  WK-YOTEISU          PIC S9(09)V99  VALUE  ZERO.
     02  WK-IRISU            PIC 9(04)V99  VALUE  ZERO.
     02  WK-CS               PIC 9(05)  VALUE  ZERO.
     02  WK-AMARI            PIC 9(05)V99  VALUE  ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0220B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    INF.
     MOVE     "INF"          TO   ERR-FL-ID.
     MOVE     IN-STATUS      TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE     "HSHOTBL"      TO   ERR-FL-ID.
     MOVE     SHO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "HMEIMS"       TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    AUTHACF.
     MOVE     "AUTHACF"      TO   ERR-FL-ID.
     MOVE     AUT-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 SHA0220B-SEC                SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 SHA0220B-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    INPUT           INF
                             HJYOKEN
                             HSHOTBL
                             HMEIMS
             I-O             AUTHACF.
*自動発注係数
     MOVE    14                  TO   JYO-F01.
     MOVE    SPACE               TO   JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY "HJYOKEN INVALID KEY = " JYO-F01 ":" JYO-F02
                      UPON CONS
             DISPLAY "ｼﾞﾄﾞｳﾊｯﾁｭｳ ｹｲｽｳ ｲｼﾞｮｳ" UPON CONS
             STOP  RUN
     END-READ.
*****　商品在庫マスタＲＥＡＤ　****
     PERFORM    INF-READ-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      _１　　商品在庫マスタＲＥＡＤ処理                      *
****************************************************************
 INF-READ-SEC               SECTION.
     READ    INF
             AT   END
             MOVE "END"     TO   END-FLG
             NOT  AT  END
             ADD   1        TO   READ-CNT
     END-READ.
 INF-READ-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
****　商品名称マスタを検索する　****
*****DISPLAY "IN-F021 = " IN-F021 UPON CONS.
*****DISPLAY "IN-F022 = " IN-F022 UPON CONS.
     MOVE    IN-F021            TO   MEI-F011.
     MOVE    IN-F022            TO   MEI-F012.
     READ    HMEIMS
         INVALID   KEY
             ADD   1             TO   MEI-CNT
             DISPLAY   "ｼﾖｳﾋﾝﾒｲｼﾖｳﾏｽﾀ INVALID  KEY = "
                        IN-F021   " "   IN-F022   UPON   CONS
             GO                  TO   MAIN-010
         NOT INVALID   KEY
             MOVE   MEI-F07      TO   WK-IRISU
     END-READ.
*名称ＣＤ４９９９９９～９０００００間が対象
     IF   MEI-F011   >                00499999   AND
          MEI-F011   <                00900000
          CONTINUE
     ELSE
          IF   MEI-F92   NOT =   1
               ADD   1                TO   JUMP-CNT
               GO                     TO   MAIN-010
          END-IF
     END-IF.
*前月売上高をセットする。
     MOVE   IN-F23               TO   WK-HACHSU.
     ADD    IN-F24               TO   WK-HACHSU.
     ADD    IN-F25               TO   WK-HACHSU.
*安全在庫数をセット
     MOVE   WK-HACHSU            TO   WK-ANZENSU.
*欠品を判定する
*現在庫＋未入庫－未出庫
     COMPUTE WK-ZAIKOSU = IN-F04 + IN-F26 - IN-F27.
*****DISPLAY "IN-F04  = " IN-F04  UPON CONS.
*****DISPLAY "IN-F26  = " IN-F26  UPON CONS.
*****DISPLAY "IN-F27  = " IN-F27  UPON CONS.
*ＷＫ安全在庫＞在庫数
*****DISPLAY "WK-ANZENSU = " WK-ANZENSU UPON CONS.
*****DISPLAY "WK-ZAIKOSU = " WK-ZAIKOSU UPON CONS.
     IF   WK-ANZENSU    >    WK-ZAIKOSU
          PERFORM    KEPIN-EDIT-WRITE-SEC
     END-IF.
****  商品在庫マスタＲＥＡＤ　****
 MAIN-010.
     PERFORM    INF-READ-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      _１　　欠品データ編集出力処理                          *
****************************************************************
 KEPIN-EDIT-WRITE-SEC        SECTION.
*自動発注ワーク初期化
     MOVE   SPACE                TO   AUT-REC.
     INITIALIZE                       AUT-REC.
*伝票区分
     MOVE   IN-F01               TO   AUT-F01.
*仕入先ＣＤ
     MOVE   MEI-F05              TO   AUT-F02.
*商品ＣＤ
     MOVE   IN-F021              TO   AUT-F031.
*品単ＣＤ
     MOVE   IN-F022              TO   AUT-F032.
*_番
     MOVE   IN-F03               TO   AUT-F04.
*現在庫数
     MOVE   IN-F04               TO   AUT-F05.
*安全在庫数
     MOVE   WK-ANZENSU           TO   AUT-F06.
*受注残数
     MOVE   IN-F27               TO   AUT-F07.
*発注残数
     MOVE   IN-F26               TO   AUT-F08.
*発注予定数／実発注数
     COMPUTE WK-YOTEISU = WK-ANZENSU - WK-ZAIKOSU.
     IF     WK-IRISU    NOT = ZERO
            DIVIDE  WK-YOTEISU   BY  WK-IRISU  GIVING   WK-CS
                                         REMAINDER  WK-AMARI
            IF      WK-AMARI   >  ZERO
                    ADD  1       TO   WK-CS
            END-IF
            COMPUTE WK-YOTEISU =  WK-IRISU  *  WK-CS
     END-IF.
     MOVE   WK-YOTEISU           TO   AUT-F09 AUT-F10.
*仕入単価
     MOVE   MEI-F041             TO   AUT-F11.
     MOVE   MEI-F042             TO   AUT-F12.
     MOVE   MEI-F043             TO   AUT-F13.
*前月売上数
     MOVE   IN-F21               TO   AUT-F18.
*発注数量１，２，３
     MOVE   IN-F23               TO   AUT-F22.
     MOVE   IN-F24               TO   AUT-F23.
     MOVE   IN-F25               TO   AUT-F24.
*入数
     MOVE   WK-IRISU             TO   AUT-F19.
*
     WRITE  AUT-REC.
     ADD   1                     TO   WRITE-CNT.
 KEPIN-EDIT-WRITE-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              INF
                        HJYOKEN
                        HSHOTBL
                        HMEIMS
                        AUTHACF.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM      IN = "  READ-CNT   UPON  CONS.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM(ｼﾖﾘ)    = "  SYORI-CNT  UPON  CONS.
     DISPLAY  "ｼｮｳﾋﾝ ﾍﾝｶﾝ TBL INV = "  SYOTB-CNT  UPON  CONS.
     DISPLAY  "ｼｮｳﾋﾝ ﾒｲｼｮｳﾏｽﾀ INV = "  MEI-CNT    UPON  CONS.
     DISPLAY  "ｼﾞﾄﾞｳﾊｯﾁｭｳ-ﾌﾗｸﾞ  0 = "  JUMP-CNT   UPON  CONS.
     DISPLAY  "ｹｯﾋﾟﾝﾃﾞ-ﾀF     OUT = "  WRITE-CNT  UPON  CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
