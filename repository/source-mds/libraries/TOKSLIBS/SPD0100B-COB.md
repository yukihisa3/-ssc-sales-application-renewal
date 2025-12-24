# SPD0100B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SPD0100B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫実績＆販売実績　　　　　　　　*
*    モジュール名　　　　：　手書取込データ量販店ＤＴ作成　　　*
*    作成日／更新日　　　：　09/10/13                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＰＣ側より受信した手書取込データを*
*                            読み、手書取込用量販店データを作成*
*                            する。　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPD0100B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<< ＰＣ側手書取込Ｆ　>>---*
     SELECT   PCDATAWK   ASSIGN    TO        DA-01-S-PCDATAWK
                        FILE    STATUS      IS   PCD-STATUS.
*
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*---<< 取引先マスタ >>-*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*---<< 商品名称マスタ >>-*
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*---<< 量販店ＪＮＬ（ＰＣ連携用）　>>-*
     SELECT   PCRYOJF   ASSIGN    TO        DA-01-VI-PCRYOJL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   PCR-F011
                                                 PCR-F012
                                                 PCR-F02
                        FILE      STATUS    IS   PCR-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<< ＰＣ側手書取込Ｆ　>>---*
 FD    PCDATAWK   BLOCK     CONTAINS  20   RECORDS.
       COPY   PCDATAWK   OF        XFDLIB
              JOINING   PCD       PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  店舗マスタ　 >>-*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  商品名称マスタ >>-*
 FD  MEIMS1.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<< 量販店ＪＮＬ（ＰＣ連携用）　>>-*
 FD  PCRYOJF.
     COPY     PCRYOJF   OF        XFDLIB
              JOINING   PCR       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  PCD-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  PCR-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  SYURYO-FLG          PIC  X(03)  VALUE SPACE.
     02  SYURYO-FLG1         PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  PCJISSF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  KAISI-FLG           PIC  X(01)  VALUE SPACE.
 01  CNT-AREA.
     02  RD-CNT              PIC  9(07)  VALUE ZERO.
     02  WT-CNT              PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE ZERO.
     02  WK-TOKCD            PIC  9(08)  VALUE ZERO.
     02  WK-MEMO1-NO         PIC  9(04)  VALUE ZERO.
     02  WK-MEMO2-NO         PIC  9(04)  VALUE ZERO.
     02  BK-PCD-F02          PIC  9(08)  VALUE ZERO.
     02  BK-PCD-F10          PIC  9(08)  VALUE ZERO.
     02  BK-PCD-F12          PIC  X(13)  VALUE ZERO.
     02  WK-PCD-F10          PIC  9(08)  VALUE ZERO.
     02  WK-YOBI-NO          PIC  9(09)  VALUE ZERO.
     02  IX                  PIC  9(03)  VALUE ZERO.
     02  IY                  PIC  9(03)  VALUE ZERO.
     02  IZ                  PIC  9(03)  VALUE ZERO.
     02  ST-MEMO-NO          PIC  9(04)  VALUE ZERO.
     02  ED-MEMO-NO          PIC  9(04)  VALUE ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SPD0100B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  SYS-TIME2               PIC  9(08).
 01  FILLER            REDEFINES  SYS-TIME2.
     03  SYS-TIMEW           PIC  9(06).
     03  FILLER              PIC  9(02).
 01  WK-SYS-TIME             PIC  9(08)  VALUE  ZERO.
*
****  ＷＲＫ領域  ***
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
     02  SYS-DATE            PIC  9(06)  VALUE  ZERO.
 01  SYS-DATE-YMD            PIC  9(08)  VALUE  ZERO.
 01  SYS-DATE-YMD1           PIC  9(09)  VALUE  ZERO.
*量販店データワーク
     COPY  PCRYOJWK  OF  XFDLIB  JOINING  PCW  AS  PREFIX.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 LINKAGE                     SECTION.
 01  PARA-BUMON            PIC X(04).
 01  PARA-TANCD            PIC X(02).
 01  PARA-MEMO1            PIC 9(04).
 01  PARA-MEMO2            PIC 9(04).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING  PARA-BUMON
                                              PARA-TANCD
                                              PARA-MEMO1
                                              PARA-MEMO2.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    PCDATAWK.
     MOVE     "PCDATAWK"     TO   ERR-FL-ID.
     MOVE     PCD-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "JYOKEN1"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTENMS.
     MOVE    "TENMS1  "      TO   ERR-FL-ID.
     MOVE     TEN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    MEIMS1.
     MOVE    "MEIMS1  "      TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    PCRYOJF.
     MOVE    "PCRYOJL1"      TO   ERR-FL-ID.
     MOVE     PCR-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 CONTROL-START               SECTION.
*システム日付の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE-YMD
                                       SYS-DATE-YMD1.
*開始時、システム時刻の取得
     ACCEPT    SYS-TIME           FROM TIME.
     MOVE      SYS-TIME           TO   SYS-TIME2.
*開始メッセージ出力
     DISPLAY "## SPD0100B START " SYS-DATE-YMD(1:4) "/"
              SYS-DATE-YMD(5:2) "/" SYS-DATE-YMD(7:2) " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ##"  UPON CONS.
* *****************************************************
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
* *****************************************************
*終了時、システム時刻の取得
     ACCEPT    SYS-TIME           FROM TIME.
     MOVE      SYS-TIME           TO   SYS-TIME2.
*終了メッセージ出力
     DISPLAY "## SPD0100B END   " SYS-DATE-YMD(1:4) "/"
              SYS-DATE-YMD(5:2) "/" SYS-DATE-YMD(7:2) " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ##"  UPON CONS.
*
     STOP      RUN.
 CONTROL-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*
     OPEN     INPUT          PCDATAWK  HTENMS  MEIMS1
              I-O            HJYOKEN  PCRYOJF.
     MOVE      ZERO               TO   WK-MEMO2-NO.
*（手書連携）手書データ連携ファイル読込
     PERFORM    READ-PCDATAWK-SEC.
*対象データ無しメッセージ出力
     IF  END-FLG = "END"
         DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*
*取引先ＣＤブレイクチェック
     IF  PCD-F02  NOT =  BK-PCD-F02
         IF   KAISI-FLG = "1"
**************量販店JNL出力
              PERFORM RYOJNL-WT-SEC
         END-IF
*********店舗テーブル初期化
         PERFORM TENPO-CLR1-SEC
*********店舗ＴＢＬ作成
         PERFORM TENPO-TBL-SEC
*********メモ_取得
         PERFORM MEMONO-GET-SEC
         IF   KAISI-FLG = SPACE
              MOVE  WK-MEMO1-NO   TO  ST-MEMO-NO
         END-IF
         MOVE  1             TO   WK-MEMO2-NO
         MOVE  PCD-F02       TO   BK-PCD-F02
         MOVE  PCD-F10       TO   BK-PCD-F10
         MOVE  PCD-F12       TO   BK-PCD-F12
         MOVE  "1"           TO   KAISI-FLG
         MOVE  ZERO          TO   WK-YOBI-NO
     END-IF.
*納品日、相手商品ＣＤブレイク確認
     MOVE      PCD-F10       TO   WK-PCD-F10.
     IF  WK-PCD-F10  NOT =  BK-PCD-F10
     OR  PCD-F12     NOT =  BK-PCD-F12
*********量販店レコードを出力する。
         PERFORM RYOJNL-WT-SEC
         MOVE  PCD-F02       TO   BK-PCD-F02
         MOVE  PCD-F10       TO   BK-PCD-F10
         MOVE  PCD-F12       TO   BK-PCD-F12
*********店舗テーブル初期化
         PERFORM TENPO-CLR1-SEC
         ADD   1             TO   WK-MEMO2-NO
         MOVE  ZERO          TO   WK-YOBI-NO
     END-IF.
*項目セット
*メモ_
     MOVE      WK-MEMO1-NO   TO   PCW-F011.
     MOVE      WK-MEMO2-NO   TO   PCW-F012.
*担当者ＣＤ
     MOVE      PARA-TANCD    TO   PCW-F03.
*出荷場所／伝発場所
     MOVE      PCD-F23       TO   PCW-F04  PCW-F05.
*発注日／納品日
     MOVE      PCD-F09       TO   PCW-F071.
     MOVE      PCD-F10       TO   PCW-F072.
*取引先ＣＤ
     MOVE      PCD-F02       TO   PCW-F11.
*量販店商品ＣＤ
     MOVE      PCD-F12       TO   PCW-F121.
*商品名称取得
     MOVE      PCD-F13       TO   MEI-F011.
     MOVE      PCD-F14       TO   MEI-F0121.
     MOVE      PCD-F15       TO   MEI-F0122.
     MOVE      PCD-F16       TO   MEI-F0123.
*商品名称マスタ読込
     PERFORM   HMEIMS-READ-SEC.
     IF  HMEIMS-INV-FLG = SPACE
               MOVE MEI-F031 TO   PCW-F1211
               MOVE MEI-F032 TO   PCW-F1212
     ELSE
               MOVE ALL "*"  TO   PCW-F1211
               MOVE ALL "*"  TO   PCW-F1212
     END-IF.
*原価単価／売価単価
     MOVE      PCD-F20       TO   PCW-F131.
     MOVE      PCD-F21       TO   PCW-F132.
*修正
     MOVE      9             TO   PCW-F573.
*サカタ商品ＣＤ／品単ＣＤ
     MOVE      PCD-F13       TO   PCW-F621.
     MOVE      PCD-F14       TO   PCW-F622(1:5).
     MOVE      PCD-F15       TO   PCW-F622(6:2).
     MOVE      PCD-F16       TO   PCW-F622(8:1).
*システム日付
     MOVE      SYS-DATE-YMD1 TO   PCW-F99.
*店舗索引→数量セット
     MOVE      SPACE         TO   SYURYO-FLG1.
     PERFORM  VARYING IX FROM 1 BY 1 UNTIL IX > 300
                                        OR  SYURYO-FLG1 = "END"
          IF  PCW-F161(IX) = PCD-F03
              ADD PCD-F18    TO   PCW-F162(IX)
              MOVE "END"     TO   SYURYO-FLG1
          END-IF
     END-PERFORM.
*
*（手書連携）手書データ連携ファイル読込
     PERFORM    READ-PCDATAWK-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*データがある場合
     IF  RD-CNT  >  ZERO
         PERFORM RYOJNL-WT-SEC
     END-IF.
*
     CLOSE           PCDATAWK  HTENMS  MEIMS1  HJYOKEN  PCRYOJF.
*
     DISPLAY  "#  PCDATAWK READ   = "  RD-CNT        UPON   CONS.
     DISPLAY  "#  PCRYOJF  WRITE  = "  WT-CNT        UPON   CONS.
*メモ_範囲パラメタセット
     MOVE     ST-MEMO-NO       TO      PARA-MEMO1.
     MOVE     ED-MEMO-NO       TO      PARA-MEMO2.
*
 END-END.
     EXIT.
****************************************************************
*      1.1 振替データ読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 READ-PCDATAWK-SEC               SECTION.
*
     READ    PCDATAWK
         AT   END
           MOVE   "END"      TO   END-FLG
           GO                TO   READ-PCDATAWK-END
         NOT  AT  END
           ADD     1         TO   RD-CNT
     END-READ.
*
     IF    RD-CNT(5:3)  =  "000 " OR "500"
           DISPLAY "READ-CNT = " RD-CNT UPON CONS
     END-IF.
*
 READ-PCDATAWK-END.
     EXIT.
****************************************************************
*      ALL        店舗マスタ読込　　　　　　                   *
****************************************************************
 HTENMS-READ-SEC        SECTION.
*
     READ     HTENMS
              NEXT  AT  END
              MOVE     "END"             TO  SYURYO-FLG
              GO                         TO  HTENMS-READ-EXIT
     END-READ.
 TOK01.
*取引先ＣＤチェック
     IF       TEN-F52  >  PCD-F02
              MOVE     "END"             TO  SYURYO-FLG
     END-IF.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        商品名称マスタ読込　　　　                   *
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     READ     MEIMS1
              INVALID     MOVE  "INV"    TO  HMEIMS-INV-FLG
              NOT INVALID MOVE  SPACE    TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        商品名称マスタ読込　　　　                   *
****************************************************************
 RYOJNL-WT-SEC          SECTION.
*基本項目転送
     MOVE     PCW-REC(1:143)    TO     PCR-REC(1:143).
     MOVE     PCW-REC(1944:66)  TO     PCR-REC(444:66).
     MOVE     PCW-F99           TO     PCR-F99.
*店舗情報転送
 RYOJNL-WT010.
*    １～５０店舗
     IF     PCW-F161(1)  NOT =  ZERO
            ADD    1          TO        WK-YOBI-NO
            MOVE   WK-YOBI-NO TO        PCR-F02
            MOVE   1          TO        IX
            PERFORM VARYING IX FROM IX BY 1 UNTIL IX > 50
                    MOVE    PCW-F161(IX)  TO PCR-F161(IX)
                    MOVE    PCW-F162(IX)  TO PCR-F162(IX)
            END-PERFORM
            WRITE PCR-REC
            ADD     1         TO        WT-CNT
     END-IF.
 RYOJNL-WT020.
*    ５１～１００店舗
     IF     PCW-F161(51)  NOT =  ZERO
            ADD    1          TO        WK-YOBI-NO
            MOVE   WK-YOBI-NO TO        PCR-F02
            MOVE    51        TO        IX
            MOVE    ZERO      TO        IY
            PERFORM VARYING IX FROM IX BY 1 UNTIL IX > 100
                    ADD     1             TO IY
                    MOVE    PCW-F161(IX)  TO PCR-F161(IY)
                    MOVE    PCW-F162(IX)  TO PCR-F162(IY)
            END-PERFORM
            WRITE PCR-REC
            ADD     1         TO        WT-CNT
     END-IF.
 RYOJNL-WT030.
*    １０１～１５０店舗
     IF     PCW-F161(101) NOT =  ZERO
            ADD    1          TO        WK-YOBI-NO
            MOVE   WK-YOBI-NO TO        PCR-F02
            MOVE    101       TO        IX
            MOVE    ZERO      TO        IY
            PERFORM VARYING IX FROM IX BY 1 UNTIL IX > 150
                    ADD     1             TO IY
                    MOVE    PCW-F161(IX)  TO PCR-F161(IY)
                    MOVE    PCW-F162(IX)  TO PCR-F162(IY)
            END-PERFORM
            WRITE PCR-REC
            ADD     1         TO        WT-CNT
     END-IF.
 RYOJNL-WT040.
*    １５１～２００店舗
     IF     PCW-F161(151) NOT =  ZERO
            ADD    1          TO        WK-YOBI-NO
            MOVE   WK-YOBI-NO TO        PCR-F02
            MOVE    151       TO        IX
            MOVE    ZERO      TO        IY
            PERFORM VARYING IX FROM IX BY 1 UNTIL IX > 200
                    ADD     1             TO IY
                    MOVE    PCW-F161(IX)  TO PCR-F161(IY)
                    MOVE    PCW-F162(IX)  TO PCR-F162(IY)
            END-PERFORM
            WRITE PCR-REC
            ADD     1         TO        WT-CNT
     END-IF.
 RYOJNL-WT050.
*    ２０１～２５０店舗
     IF     PCW-F161(201) NOT =  ZERO
            ADD    1          TO        WK-YOBI-NO
            MOVE   WK-YOBI-NO TO        PCR-F02
            MOVE    201       TO        IX
            MOVE    ZERO      TO        IY
            PERFORM VARYING IX FROM IX BY 1 UNTIL IX > 250
                    ADD     1             TO IY
                    MOVE    PCW-F161(IX)  TO PCR-F161(IY)
                    MOVE    PCW-F162(IX)  TO PCR-F162(IY)
            END-PERFORM
            WRITE PCR-REC
            ADD     1         TO        WT-CNT
     END-IF.
 RYOJNL-WT060.
*    ２５１～３００店舗
     IF     PCW-F161(251) NOT =  ZERO
            DISPLAY "AAAAA" UPON CONS
            ADD    1          TO        WK-YOBI-NO
            MOVE   WK-YOBI-NO TO        PCR-F02
            MOVE    251       TO        IX
            MOVE    ZERO      TO        IY
            PERFORM VARYING IX FROM IX BY 1 UNTIL IX > 300
                    ADD     1             TO IY
                    MOVE    PCW-F161(IX)  TO PCR-F161(IY)
                    MOVE    PCW-F162(IX)  TO PCR-F162(IY)
            END-PERFORM
            WRITE PCR-REC
            ADD     1         TO        WT-CNT
     END-IF.
*
 RYOJNL-WT-EXIT.
     EXIT.
****************************************************************
*      ALL     メモ_取得                                      *
****************************************************************
 MEMONO-GET-SEC              SECTION.
*
*----<< メモ_取得 >>-*
     MOVE    54              TO   JYO-F01.
     MOVE    "PCDATAWK"      TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN RYOUHAN INVALID " JYO-F01 " - "
                      JYO-F02 UPON CONS
             MOVE     4000   TO   PROGRAM-STATUS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-MEMO1-NO
             ADD      1           TO   WK-MEMO1-NO
             IF  WK-MEMO1-NO > JYO-F06
                 MOVE JYO-F05     TO   WK-MEMO1-NO
             END-IF
     END-READ.
*
     MOVE    WK-MEMO1-NO          TO   JYO-F04   ED-MEMO-NO.
*条件ファイル開放
     REWRITE  JYO-REC.
*
 MEMONO-GET-EXIT.
     EXIT.
****************************************************************
*      ALL     店舗テーブル作成　                              *
****************************************************************
 TENPO-TBL-SEC               SECTION.
*
     MOVE   SPACE            TO   TEN-REC.
     INITIALIZE                   TEN-REC.
*店舗マスタスタート
     MOVE   PCD-F02          TO   TEN-F52.
     START  HTENMS  KEY  IS  >=   TEN-F52  TEN-F011
            INVALID
            DISPLAY "HTENMS INVALID KEY = " TEN-F52 UPON CONS
            MOVE     4000    TO   PROGRAM-STATUS
            STOP RUN
     END-START.
*店舗マスタ読込
     MOVE    SPACE           TO   PCW-REC.
     INITIALIZE                   PCW-REC.
     MOVE    SPACE           TO   SYURYO-FLG.
     PERFORM HTENMS-READ-SEC.
*終了ＦＬＧが”ＥＮＤ”の場合
     IF  SYURYO-FLG = "END"
         DISPLAY "HTENMS TBL ERROR = " TEN-F52  UPON CONS
         MOVE     4000       TO   PROGRAM-STATUS
         STOP RUN
     END-IF.
*店舗テーブル作成
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 300
                                  OR SYURYO-FLG = "END"
             MOVE  TEN-F011  TO   PCW-F161(IX)
             PERFORM HTENMS-READ-SEC
     END-PERFORM.
*
 TENPO-TBL-EXIT.
     EXIT.
****************************************************************
*      ALL     店舗テーブル初期化                            *
****************************************************************
 TENPO-CLR-SEC               SECTION.
*
*店舗テーブル初期化
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 300
             MOVE  ZERO      TO   PCW-F161(IX)
             MOVE  ZERO      TO   PCW-F162(IX)
     END-PERFORM.
*
 TENPO-CLR-EXIT.
     EXIT.
****************************************************************
*      ALL     店舗テーブル初期化                            *
****************************************************************
 TENPO-CLR1-SEC              SECTION.
*
*店舗テーブル初期化
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 300
             MOVE  ZERO      TO   PCW-F162(IX)
     END-PERFORM.
*
 TENPO-CLR1-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
