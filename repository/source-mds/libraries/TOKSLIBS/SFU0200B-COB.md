# SFU0200B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SFU0200B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　日次振替（データ編集／振分け）　　*
*    作成日／更新日　　　：　2000/06/21                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　振替データを編集し、各部門毎にデー*
*                            タを振り分ける。                  *
*2002/05/13 伝区＝４１，５１の時、更新区分を修正               *
*2002/08/28 伝区＝３０（作業実績の時）　更新区分を修正         *
*2003/06/17 組織変更による部門ＣＤ変更対応                     *
*2005/06/10 組織変更による部門ＣＤ変更対応                     *
*2007/01/10 ストック■外し処理作成　　　　　　                 *
*2007/03/28 ストック■外し処理不具合修正　　　                 *
*2007/05/14 ストック■外し処理不具合修正　　　                 *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU0200B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＴＯＫＵファイル  >>---*
     SELECT   TOKU      ASSIGN    TO        DA-01-S-TOKU
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TOK-STATUS.
*
*---<<  本社分  >>---*
     SELECT   HON       ASSIGN    TO        DA-01-S-HON
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HON-STATUS.
*
*---<<  福岡分  >>---*
     SELECT   FUK       ASSIGN    TO        DA-01-S-FUK
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   FUK-STATUS.
*
*---<<  仙台分  >>---*
     SELECT   SEN       ASSIGN    TO        DA-01-S-SEN
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SEN-STATUS.
*
*---<<  岡山分  >>---*
     SELECT   OKA       ASSIGN    TO        DA-01-S-OKA
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OKA-STATUS.
*
*---<<  北海道分  >>---*
     SELECT   HOK       ASSIGN    TO        DA-01-S-HOK
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HOK-STATUS.
*
*---<<  大阪分  >>---*
     SELECT   OSA       ASSIGN    TO        DA-01-S-OSA
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OSA-STATUS.
*
*---<<  本社ストック外し  >>---*
     SELECT   SHO       ASSIGN    TO        DA-01-S-SHO
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SHO-STATUS.
*
*---<<  九州ストック外し  >>---*
     SELECT   SKY       ASSIGN    TO        DA-01-S-SKY
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SKY-STATUS.
*
*---<<  部門取引先マスタ  >>---*
     SELECT   BUTOKMF1  ASSIGN    TO        DA-01-VI-BUTOKMF1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   BU1-F01
                        FILE      STATUS    IS   BU1-STATUS.
*
*---<<  部門取引先マスタ  >>---*
     SELECT   BUTOKMF2  ASSIGN    TO        DA-01-VI-BUTOKMF2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   BU2-F03
                        FILE      STATUS    IS   BU2-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＯＫＵファイル  >>---*
 FD  TOKU     BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAE   OF        XFDLIB
              JOINING   TO0       PREFIX.
*---<<  本社ファイル  >>---*
 FD  HON      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   HON       PREFIX.
*---<<  福岡ファイル  >>---*
 FD  FUK      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   FUK       PREFIX.
*---<<  仙台ファイル  >>---*
 FD  SEN      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   SEN       PREFIX.
*---<<  岡山ファイル  >>---*
 FD  OKA      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   OKA       PREFIX.
*---<<  北海道ファイル  >>---*
 FD  HOK      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   HOK       PREFIX.
*---<<  大阪ファイル  >>---*
 FD  OSA      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   OSA       PREFIX.
*---<<  本社ストック外し  >>---*
 FD  SHO      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  九州ストック外し  >>---*
 FD  SKY      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   SKY       PREFIX.
*---<<  部門取引先マスタ１  >>---*
 FD  BUTOKMF1.
     COPY     BUTOKMF   OF        XFDLIB
              JOINING   BU1       PREFIX.
*----<< 部門取引先マスタ２  >>-*
 FD  BUTOKMF2.
     COPY     BUTOKMF   OF        XFDLIB
              JOINING   BU2       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  TOK-STATUS          PIC  X(02).
     02  HON-STATUS          PIC  X(02).
     02  FUK-STATUS          PIC  X(02).
     02  SEN-STATUS          PIC  X(02).
     02  OKA-STATUS          PIC  X(02).
     02  HOK-STATUS          PIC  X(02).
     02  OSA-STATUS          PIC  X(02).
     02  SHO-STATUS          PIC  X(02).
     02  SKY-STATUS          PIC  X(02).
     02  BU1-STATUS          PIC  X(02).
     02  BU2-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  SKIP-FLG            PIC  X(01)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  HON-CNT             PIC  9(07)  VALUE ZERO.
     02  FUK-CNT             PIC  9(07)  VALUE ZERO.
     02  SEN-CNT             PIC  9(07)  VALUE ZERO.
     02  OKA-CNT             PIC  9(07)  VALUE ZERO.
     02  HOK-CNT             PIC  9(07)  VALUE ZERO.
     02  OSA-CNT             PIC  9(07)  VALUE ZERO.
     02  SHO-CNT             PIC  9(07)  VALUE ZERO.
     02  SKY-CNT             PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE ZERO.
****  ＷＲＫ領域  *** 1999/12/27 NAV START
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
****  部門ＦＬＧ
 01  WK-BUMON.
     02  WK-BUMON-1          PIC  X(01)  VALUE  SPACE.
     02  WK-BUMON-2          PIC  X(01)  VALUE  SPACE.
****  部門ＣＤ変換
 01  WK-BUMON-HEN.
     02  WK-BUMON-HEN-1      PIC  X(04)  VALUE  SPACE.
     02  WK-BUMON-HEN-2      PIC  X(04)  VALUE  SPACE.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SFU0200B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*ワークＣＯＰＹ句定義
     COPY  FURIKAF  OF  XFDLIB  JOINING  WRK  PREFIX.
     COPY  FURIKAF  OF  XFDLIB  JOINING  WRK1 PREFIX.
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    TOKU.
     MOVE     "TOKU"         TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HON.
     MOVE     "HON"          TO   ERR-FL-ID.
     MOVE     HON-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    FUK.
     MOVE     "FUK"          TO   ERR-FL-ID.
     MOVE     FUK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SEN.
     MOVE     "SEN"          TO   ERR-FL-ID.
     MOVE     SEN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    OKA.
     MOVE     "OKA"          TO   ERR-FL-ID.
     MOVE     OKA-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC6                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HOK.
     MOVE     "HOK"          TO   ERR-FL-ID.
     MOVE     HOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC7                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    OSA.
     MOVE     "OSA"          TO   ERR-FL-ID.
     MOVE     OSA-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC8                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SHO.
     MOVE     "SHO"          TO   ERR-FL-ID.
     MOVE     SHO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC9                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SKY.
     MOVE     "SKY"          TO   ERR-FL-ID.
     MOVE     SKY-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC10               SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    BUTOKMF1.
     MOVE     "BUTOKMF1"     TO   ERR-FL-ID.
     MOVE     BU1-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC11               SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    BUTOKMF2.
     MOVE     "BUTOKMF2"     TO   ERR-FL-ID.
     MOVE     BU2-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    INPUT           TOKU  BUTOKMF1  BUTOKMF2.
     OPEN    OUTPUT          HON  FUK  SEN  HOK  OSA  OKA.
     OPEN    OUTPUT          SHO  SKY.
*振替データ初期読込み
     PERFORM TOKU-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      電算室殿⇒振替データファイル読込み                      *
****************************************************************
 TOKU-READ-SEC               SECTION.
*
     READ    TOKU
             AT  END
             MOVE   "END"      TO   END-FLG
             NOT AT END
             ADD     1         TO   READ-CNT
     END-READ.
*
 TOKU-READ-EXIT.
     EXIT.
****************************************************************
*      ■０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*フラグ初期化
     MOVE         SPACE        TO   SKIP-FLG.
*ワーク初期化
     MOVE         SPACE        TO   WRK-REC WRK1-REC.
     INITIALIZE                     WRK-REC WRK1-REC.
*判定処理
     EVALUATE   TO0-F02
         WHEN   20
                PERFORM  FURI-20-SEC
         WHEN   70
                IF  TO0-F01 = 2910 OR 2920 OR 2930 OR 2940
                    MOVE    "1"   TO     WK-BUMON-1
                ELSE
                    MOVE    "2"   TO     WK-BUMON-1
                END-IF
                IF  TO0-F06(5:4) = 2910 OR 2920 OR 2930 OR
                              2940
                    MOVE    "1"   TO     WK-BUMON-2
                ELSE
                    MOVE    "2"   TO     WK-BUMON-2
                END-IF
                EVALUATE WK-BUMON-1 ALSO WK-BUMON-2
*                   特販部内の場合
                    WHEN "1"        ALSO "1"
                          PERFORM  FURI-702-SEC
*                   特販部＝＞他部へ移動
                    WHEN "1"        ALSO "2"
                          PERFORM  FURI-70-SEC
*                   他部＝＞特販部へ移動
                    WHEN "2"        ALSO "1"
                          PERFORM  FURI-701-SEC
                END-EVALUATE
         WHEN   71
                IF  TO0-F01 = 2910 OR 2920 OR 2930 OR 2940
                    MOVE    "1"   TO     WK-BUMON-1
                ELSE
                    MOVE    "2"   TO     WK-BUMON-1
                END-IF
                IF  TO0-F06(5:4) = 2910 OR 2920 OR 2930 OR
                              2940
                    MOVE    "1"   TO     WK-BUMON-2
                ELSE
                    MOVE    "2"   TO     WK-BUMON-2
                END-IF
                EVALUATE WK-BUMON-1 ALSO WK-BUMON-2
*                   特販部内の場合
                    WHEN "1"        ALSO "1"
                          PERFORM  FURI-712-SEC
*                   特販部＝＞他部へ移動
                    WHEN "1"        ALSO "2"
                          PERFORM  FURI-71-SEC
*                   他部＝＞特販部へ移動
                    WHEN "2"        ALSO "1"
                          PERFORM  FURI-711-SEC
                END-EVALUATE
         WHEN   35
                PERFORM FURI-35-SEC
         WHEN   36
                PERFORM FURI-36-SEC
         WHEN   40
                PERFORM FURI-40-SEC
         WHEN   41
                PERFORM FURI-41-SEC
         WHEN   50
                PERFORM FURI-50-SEC
         WHEN   51
                PERFORM FURI-51-SEC
         WHEN   30
                PERFORM FURI-30-SEC
         WHEN   31
                PERFORM FURI-31-SEC
         WHEN   32
                PERFORM FURI-32-SEC
         WHEN   OTHER
                MOVE     "1"     TO   SKIP-FLG
     END-EVALUATE.
*
     IF  SKIP-FLG  =  "1"
         GO     TO               MAIN-010
     END-IF.
*更新判定
     EVALUATE   WRK-F01
         WHEN   "2920"
                MOVE  WRK-REC   TO   HON-REC
                WRITE HON-REC
                ADD   1         TO   HON-CNT
         WHEN   "2940"
                MOVE  WRK-REC   TO   FUK-REC
                WRITE FUK-REC
                ADD   1         TO   FUK-CNT
         WHEN   "2921"
                MOVE  WRK-REC   TO   SEN-REC
                WRITE SEN-REC
                ADD   1         TO   SEN-CNT
         WHEN   "2931"
                MOVE  WRK-REC   TO   OKA-REC
                WRITE OKA-REC
                ADD   1         TO   OKA-CNT
         WHEN   "2910"
                MOVE  WRK-REC   TO   HON-REC
                WRITE HON-REC
                ADD   1         TO   HON-CNT
         WHEN   "2930"
                MOVE  WRK-REC   TO   HON-REC
                WRITE HON-REC
                ADD   1         TO   HON-CNT
         WHEN   OTHER
                ADD   1         TO   SKIP-CNT
     END-EVALUATE.
*
     IF  WRK1-F01  NOT =  SPACE
         EVALUATE   WRK1-F01
             WHEN   "2920"
                    MOVE  WRK1-REC  TO   HON-REC
                    WRITE HON-REC
                    ADD   1         TO   HON-CNT
             WHEN   "2940"
                    MOVE  WRK1-REC  TO   FUK-REC
                    WRITE FUK-REC
                    ADD   1         TO   FUK-CNT
             WHEN   "2921"
                    MOVE  WRK1-REC  TO   SEN-REC
                    WRITE SEN-REC
                    ADD   1         TO   SEN-CNT
             WHEN   "2931"
                    MOVE  WRK1-REC  TO   OKA-REC
                    WRITE OKA-REC
                    ADD   1         TO   OKA-CNT
             WHEN   "2910"
                    MOVE  WRK1-REC  TO   HON-REC
                    WRITE HON-REC
                    ADD   1         TO   HON-CNT
             WHEN   "2930"
                    MOVE  WRK1-REC  TO   HON-REC
                    WRITE HON-REC
                    ADD   1         TO   HON-CNT
             WHEN   OTHER
                    ADD   1         TO   SKIP-CNT
         END-EVALUATE
     END-IF.
*
 MAIN-010.
     PERFORM TOKU-READ-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*    振替データ（発注）                                        *
****************************************************************
 FURI-20-SEC                 SECTION.
*部門ＣＤ
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分
     MOVE   "3"              TO   WRK-F16.
*場所
     MOVE   TO0-F18          TO   WRK-F17.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
*
 FURI-20-EXIT.
     EXIT.
****************************************************************
*    振替データ（振替）特販部＝＞特販部                        *
****************************************************************
 FURI-702-SEC                SECTION.
*## 出庫元データ ##
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票番号
     MOVE   TO0-F03          TO   WRK-F03.
*行番号
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分
*## 2002/03/04 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*場所
     MOVE   TO0-F18          TO   WRK-F17.
*社内発注番号
     MOVE   TO0-F35          TO   WRK-F18.
*完納区分
     MOVE   TO0-F36          TO   WRK-F20.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
*ストック■
     MOVE   TO0-F13          TO   WRK-F24.
*ストック_外し対応（ストックナンバーをつける）
     IF  TO0-F01 = 2910 OR 2920 OR 2930
         MOVE  SPACE     TO   SHO-REC
         INITIALIZE           SHO-REC
         MOVE  WRK-REC   TO   SHO-REC
         MOVE  "2"       TO   SHO-FIL(1:1)
*********WRITE SHO-REC
         ADD   1         TO   SHO-CNT
     END-IF.
     IF  TO0-F01 = 2940
         MOVE  SPACE     TO   SKY-REC
         INITIALIZE           SKY-REC
         MOVE  WRK-REC   TO   SKY-REC
         MOVE  "2"       TO   SKY-FIL(1:1)
*********WRITE SKY-REC
         ADD   1         TO   SKY-CNT
     END-IF.

*## 入庫元データ ##
*部門コード取得
     MOVE   TO0-F06(5:4)     TO   WRK1-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK1-F02.
*伝票番号
     MOVE   TO0-F03          TO   WRK1-F03.
*行番号
     MOVE   TO0-F04          TO   WRK1-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK1-F05.
*振替元／振替先
     MOVE   "9999"           TO   WK-BUMON-HEN-1.
     MOVE   TO0-F01          TO   WK-BUMON-HEN-2.
     MOVE   WK-BUMON-HEN     TO   WRK1-F06.
*発注日
     MOVE   TO0-F08          TO   WRK1-F07.
*納品日
     MOVE   TO0-F09          TO   WRK1-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK1-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK1-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK1-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK1-F12.
     MOVE   TO0-F15          TO   WRK1-F13.
     MOVE   TO0-F16          TO   WRK1-F14.
*    MOVE   TO0-F17          TO   WRK1-F15.
*入庫／出庫／発注区分
*## 2002/03/04 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK1-F16
     ELSE
            MOVE   "2"       TO   WRK1-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*場所
     MOVE   TO0-F21          TO   WRK1-F17.
*社内発注番号
     MOVE   TO0-F35          TO   WRK1-F18.
*完納区分
     MOVE   TO0-F36          TO   WRK1-F20.
*マスタ区分
     MOVE   "3"              TO   WRK1-F20.
*ストック■
     MOVE   TO0-F13          TO   WRK1-F24.
*ストック_外し対応（ストックナンバーをはずす）
     IF  TO0-F06(5:4) = 2910 OR 2920 OR 2930
         MOVE  SPACE     TO   SHO-REC
         INITIALIZE           SHO-REC
         MOVE  WRK1-REC  TO   SHO-REC
         MOVE  "1"       TO   SHO-FIL(1:1)
         WRITE SHO-REC
         ADD   1         TO   SHO-CNT
     END-IF.
     IF  TO0-F06(5:4) = 2940
         MOVE  SPACE     TO   SKY-REC
         INITIALIZE           SKY-REC
         MOVE  WRK1-REC  TO   SKY-REC
         MOVE  "1"       TO   SKY-FIL(1:1)
         WRITE SKY-REC
         ADD   1         TO   SKY-CNT
     END-IF.

 FURI-702-EXIT.
     EXIT.
****************************************************************
*    振替データ（振替）他部＞特販部                            *
****************************************************************
 FURI-701-SEC                SECTION.
*部門コード取得
**   MOVE   TO0-F06          TO    BU1-F01.
**   READ   BUTOKMF1
**          INVALID
**          DISPLAY "BUMON-CD INVALID = " TO0-F06  UPON CONS
**          STOP RUN
**          NOT INVALID
**          MOVE     BU1-F03 TO    WRK-F01
**   END-READ.
     MOVE   TO0-F06(5:4)     TO    WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F01          TO   BU2-F03.
     READ   BUTOKMF2
            INVALID
            MOVE     SPACE   TO   WRK-F06
            NOT INVALID
            MOVE     BU2-F01 TO   WRK-F06
     END-READ.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分
*****MOVE          "1"       TO   WRK-F16.
*## 2002/03/04 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*場所
     MOVE   TO0-F21          TO   WRK-F17.
*社内発注■
     MOVE   TO0-F35          TO   WRK-F18.
*完納区分
     MOVE   TO0-F36          TO   WRK-F20.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
*ストック■
     MOVE   TO0-F13          TO   WRK-F24.
*ストック_外し対応（ストックナンバーをはずす）
     IF  TO0-F06(5:4) = 2910 OR 2920 OR 2930
         MOVE  SPACE     TO   SHO-REC
         INITIALIZE           SHO-REC
         MOVE  WRK-REC   TO   SHO-REC
         MOVE  "1"       TO   SHO-FIL(1:1)
         WRITE SHO-REC
         ADD   1         TO   SHO-CNT
     END-IF.
     IF  TO0-F06(5:4) = 2940
         MOVE  SPACE     TO   SKY-REC
         INITIALIZE           SKY-REC
         MOVE  WRK-REC   TO   SKY-REC
         MOVE  "1"       TO   SKY-FIL(1:1)
         WRITE SKY-REC
         ADD   1         TO   SKY-CNT
     END-IF.
 FURI-701-EXIT.
     EXIT.
****************************************************************
*    振替データ（振替データ）特販部⇒他部                      *
****************************************************************
 FURI-70-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/04 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*場所
     MOVE   TO0-F18          TO   WRK-F17.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.

*ストック_外し対応
*    IF  TO0-F01 = 2910 OR 2920 OR 2930
*        MOVE  SPACE     TO   SHO-REC
*        INITIALIZE           SHO-REC
*        MOVE  WRK-REC   TO   SHO-REC
*        MOVE  "2"       TO   SHO-FIL(1:1)
*        WRITE SHO-REC
*        ADD   1         TO   SHO-CNT
*    END-IF.
*    IF  TO0-F01 = 2940
*        MOVE  SPACE     TO   SKY-REC
*        INITIALIZE           SKY-REC
*        MOVE  WRK-REC   TO   SKY-REC
*        MOVE  "2"       TO   SKY-FIL(1:1)
*        WRITE SKY-REC
*        ADD   1         TO   SKY-CNT
*    END-IF.
 FURI-70-EXIT.
     EXIT.
****************************************************************
*    振替データ（振替返品）特販部＝＞特販部                    *
****************************************************************
 FURI-712-SEC                SECTION.
*## 出庫元データ ##
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票番号
     MOVE   TO0-F03          TO   WRK-F03.
*行番号
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分
*## 2002/03/04 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*場所
     MOVE   TO0-F18          TO   WRK-F17.
*社内発注番号
     MOVE   TO0-F35          TO   WRK-F18.
*完納区分
     MOVE   TO0-F36          TO   WRK-F20.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
*ストック■
     MOVE   TO0-F13          TO   WRK-F24.

*ストック_外し対応（ストックナンバーをつける）
     IF  TO0-F01 = 2910 OR 2920 OR 2930
         MOVE  SPACE     TO   SHO-REC
         INITIALIZE           SHO-REC
         MOVE  WRK-REC   TO   SHO-REC
         MOVE  "2"       TO   SHO-FIL(1:1)
         WRITE SHO-REC
         ADD   1         TO   SHO-CNT
     END-IF.
     IF  TO0-F01 = 2940
         MOVE  SPACE     TO   SKY-REC
         INITIALIZE           SKY-REC
         MOVE  WRK-REC   TO   SKY-REC
         MOVE  "2"       TO   SKY-FIL(1:1)
         WRITE SKY-REC
         ADD   1         TO   SKY-CNT
     END-IF.
*## 入庫元データ ##
*部門コード取得
     MOVE   TO0-F06(5:4)     TO   WRK1-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK1-F02.
*伝票番号
     MOVE   TO0-F03          TO   WRK1-F03.
*行番号
     MOVE   TO0-F04          TO   WRK1-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK1-F05.
*振替元／振替先
     MOVE   "9999"           TO   WK-BUMON-HEN-1.
     MOVE   TO0-F01          TO   WK-BUMON-HEN-2.
     MOVE   WK-BUMON-HEN     TO   WRK1-F06.
*発注日
     MOVE   TO0-F08          TO   WRK1-F07.
*納品日
     MOVE   TO0-F09          TO   WRK1-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK1-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK1-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK1-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK1-F12.
     MOVE   TO0-F15          TO   WRK1-F13.
     MOVE   TO0-F16          TO   WRK1-F14.
     MOVE   TO0-F17          TO   WRK1-F15.
*入庫／出庫／発注区分
*## 2002/03/04 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK1-F16
     ELSE
            MOVE   "1"       TO   WRK1-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*場所
     MOVE   TO0-F21          TO   WRK1-F17.
*社内発注番号
     MOVE   TO0-F35          TO   WRK1-F18.
*完納区分
     MOVE   TO0-F36          TO   WRK1-F20.
*マスタ区分
     MOVE   "3"              TO   WRK1-F20.
*ストック■
     MOVE   TO0-F13          TO   WRK1-F24.

*ストック_外し対応（ストックナンバーをはずす）
     IF  TO0-F06(5:4) = 2910 OR 2920 OR 2930
         MOVE  SPACE     TO   SHO-REC
         INITIALIZE           SHO-REC
         MOVE  WRK1-REC  TO   SHO-REC
         MOVE  "1"       TO   SHO-FIL(1:1)
         WRITE SHO-REC
         ADD   1         TO   SHO-CNT
     END-IF.
     IF  TO0-F06(5:4) = 2940
         MOVE  SPACE     TO   SKY-REC
         INITIALIZE           SKY-REC
         MOVE  WRK1-REC  TO   SKY-REC
         MOVE  "1"       TO   SKY-FIL(1:1)
         WRITE SKY-REC
         ADD   1         TO   SKY-CNT
     END-IF.
 FURI-712-EXIT.
     EXIT.
****************************************************************
*    振替データ（振替返品）他部⇒特販部                        *
****************************************************************
 FURI-711-SEC                SECTION.
*部門コード取得
**   MOVE   TO0-F06          TO    BU1-F01.
**   READ   BUTOKMF1
**          INVALID
**          DISPLAY "BUMON-CD INVALID = " TO0-F06  UPON CONS
**          STOP RUN
**          NOT INVALID
**          MOVE     BU1-F03 TO    WRK-F01
**   END-READ.
     MOVE   TO0-F06(5:4)     TO    WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
     MOVE   TO0-F01          TO   BU2-F03.
     READ   BUTOKMF2
            INVALID
            MOVE     SPACE   TO   WRK-F06
            NOT INVALID
            MOVE     BU2-F01 TO   WRK-F06
     END-READ.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分
*****MOVE   "1"              TO   WRK-F16.
*## 2002/03/04 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*場所
     MOVE   TO0-F21          TO   WRK-F17.
*社内発注■
     MOVE   TO0-F35          TO   WRK-F18.
*完納区分
     MOVE   TO0-F36          TO   WRK-F20.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
*ストック■
     MOVE   TO0-F13          TO   WRK-F24.
*ストック_外し対応（ストックナンバーをはずす）
     IF  TO0-F06(5:4) = 2910 OR 2920 OR 2930
         MOVE  SPACE     TO   SHO-REC
         INITIALIZE           SHO-REC
         MOVE  WRK-REC   TO   SHO-REC
         MOVE  "1"       TO   SHO-FIL(1:1)
         WRITE SHO-REC
         ADD   1         TO   SHO-CNT
     END-IF.
     IF  TO0-F06(5:4) = 2940
         MOVE  SPACE     TO   SKY-REC
         INITIALIZE           SKY-REC
         MOVE  WRK-REC   TO   SKY-REC
         MOVE  "1"       TO   SKY-FIL(1:1)
         WRITE SKY-REC
         ADD   1         TO   SKY-CNT
     END-IF.
 FURI-711-EXIT.
     EXIT.
****************************************************************
*    振替データ（振替返品）特販部⇒他部                        *
****************************************************************
 FURI-71-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/04 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*場所
     MOVE   TO0-F18          TO   WRK-F17.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
*ストック_外し対応
*    IF  TO0-F01 = 2910 OR 2920 OR 2930
*        MOVE  SPACE     TO   SHO-REC
*        INITIALIZE           SHO-REC
*        MOVE  WRK-REC   TO   SHO-REC
*        MOVE  "2"       TO   SHO-FIL(1:1)
*        WRITE SHO-REC
*        ADD   1         TO   SHO-CNT
*    END-IF.
*    IF  TO0-F01 = 2940
*        MOVE  SPACE     TO   SKY-REC
*        INITIALIZE           SKY-REC
*        MOVE  WRK-REC   TO   SKY-REC
*        MOVE  "2"       TO   SKY-FIL(1:1)
*        WRITE SKY-REC
*        ADD   1         TO   SKY-CNT
*    END-IF.
 FURI-71-EXIT.
     EXIT.
****************************************************************
*    振替データ（３５：移動）                                  *
****************************************************************
 FURI-35-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*出庫データ振替場所取得
*    MOVE   TO0-F01          TO   BU2-F03.
*    READ   BUTOKMF2
*           INVALID
*           DISPLAY "BUMON-CD INVALID = " TO0-F01  UPON CONS
*           STOP RUN
*           NOT INVALID
*           MOVE     BU2-F01 TO   WRK1-F06
*    END-READ.
     EVALUATE TO0-F01
         WHEN 2920
              MOVE 99992920  TO  WRK1-F06
         WHEN 2940
              MOVE 99992940  TO  WRK1-F06
         WHEN 2921
              MOVE 99992921  TO  WRK1-F06
         WHEN 2930
              MOVE 99992930  TO  WRK1-F06
         WHEN 2931
              MOVE 99992931  TO  WRK1-F06
         WHEN 2910
              MOVE 99992910  TO  WRK1-F06
         WHEN 2960
              MOVE 99992960  TO  WRK1-F06
         WHEN 2970
              MOVE 99992970  TO  WRK1-F06
         WHEN 2980
              MOVE 99992980  TO  WRK1-F06
         WHEN 2930
              MOVE 99992930  TO  WRK1-F06
     END-EVALUATE.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02 WRK1-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03 WRK1-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04 WRK1-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05 WRK1-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
     MOVE   TO0-F06          TO   BU1-F01.
     READ   BUTOKMF1
            INVALID
            MOVE     SPACE   TO   WRK1-F01
            NOT INVALID
            MOVE     BU1-F03 TO   WRK1-F01
     END-READ.
*発注日
     MOVE   TO0-F08          TO   WRK-F07 WRK1-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08 WRK1-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09 WRK1-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10 WRK1-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11 WRK1-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12 WRK1-F12.
     MOVE   TO0-F15          TO   WRK-F13 WRK1-F13.
     MOVE   TO0-F16          TO   WRK-F14 WRK1-F14.
     MOVE   TO0-F17          TO   WRK-F15 WRK1-F15.
*入庫／出庫／発注区分－場所
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*入庫／出庫／発注区分－場所
*****MOVE   "1"              TO   WRK1-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK1-F16
     ELSE
            MOVE   "2"       TO   WRK1-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F21          TO   WRK1-F17.
*マスタ区分
     MOVE   "3"              TO   WRK-F20 WRK1-F20.
 FURI-35-EXIT.
     EXIT.
****************************************************************
*    振替データ（３６：移動返品）                              *
****************************************************************
 FURI-36-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*出庫データ振替場所取得
*    MOVE   TO0-F01          TO   BU2-F03.
*    READ   BUTOKMF2
*           INVALID
*           DISPLAY "BUMON-CD INVALID = " TO0-F01  UPON CONS
*           STOP RUN
*           NOT INVALID
*           MOVE     BU2-F01 TO   WRK1-F06
*    END-READ.
     EVALUATE TO0-F01
         WHEN 2920
              MOVE 99992920  TO  WRK1-F06
         WHEN 2940
              MOVE 99992940  TO  WRK1-F06
         WHEN 2921
              MOVE 99992921  TO  WRK1-F06
         WHEN 2930
              MOVE 99999293  TO  WRK1-F06
         WHEN 2931
              MOVE 99992931  TO  WRK1-F06
         WHEN 2910
              MOVE 99992910  TO  WRK1-F06
         WHEN 2960
              MOVE 99999296  TO  WRK1-F06
         WHEN 2970
              MOVE 99999297  TO  WRK1-F06
         WHEN 2980
              MOVE 99999298  TO  WRK1-F06
         WHEN 2930
              MOVE 99992930  TO  WRK1-F06
     END-EVALUATE.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02 WRK1-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03 WRK1-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04 WRK1-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05 WRK1-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
     MOVE   TO0-F06          TO   BU1-F01.
     READ   BUTOKMF1
            INVALID
            MOVE     SPACE   TO   WRK1-F01
            NOT INVALID
            MOVE     BU1-F03 TO   WRK1-F01
     END-READ.
*発注日
     MOVE   TO0-F08          TO   WRK-F07 WRK1-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08 WRK1-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09 WRK1-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10 WRK1-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11 WRK1-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12 WRK1-F12.
     MOVE   TO0-F15          TO   WRK-F13 WRK1-F13.
     MOVE   TO0-F16          TO   WRK-F14 WRK1-F14.
     MOVE   TO0-F17          TO   WRK-F15 WRK1-F15.
*入庫／出庫／発注区分－場所
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*入庫／出庫／発注区分－場所
*****MOVE   "1"              TO   WRK1-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK1-F16
     ELSE
            MOVE   "2"       TO   WRK1-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F21          TO   WRK1-F17.
*マスタ区分
     MOVE   "3"              TO   WRK-F20 WRK1-F20.
 FURI-36-EXIT.
     EXIT.
****************************************************************
*    振替データ（４０：売上）                                  *
****************************************************************
 FURI-40-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分－場所
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
 FURI-40-EXIT.
     EXIT.
****************************************************************
*    振替データ（４１：売上返品）                              *
****************************************************************
 FURI-41-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分－場所
*****MOVE   "1"              TO   WRK-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
 FURI-41-EXIT.
     EXIT.
****************************************************************
*    振替データ（５０：仕入）                                  *
****************************************************************
 FURI-50-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分－場所
*****MOVE   "1"              TO   WRK-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*マスタ区分
     MOVE   "2"              TO   WRK-F20.
 FURI-50-EXIT.
     EXIT.
****************************************************************
*    振替データ（５１：仕入返品）                              *
****************************************************************
 FURI-51-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分－場所
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*マスタ区分
     MOVE   "2"              TO   WRK-F20.
 FURI-51-EXIT.
     EXIT.
****************************************************************
*    振替データ（３０：作業実績）                              *
****************************************************************
 FURI-30-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分－場所
*## 2002/08/28 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   TO0-F19   TO   WRK-F16
     ELSE
            IF  TO0-F19  =  "1"
                MOVE "2"     TO   WRK-F16
            END-IF
            IF  TO0-F19  =  "2"
                MOVE "1"     TO   WRK-F16
            END-IF
     END-IF.
*****MOVE   TO0-F19          TO   WRK-F16.
     MOVE   TO0-F18          TO   WRK-F17.
*作業区分
     MOVE   TO0-F21          TO   WRK-F22.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
 FURI-30-EXIT.
     EXIT.
****************************************************************
*    振替データ（３１：作業実績）                              *
****************************************************************
 FURI-31-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分－場所
*****MOVE   "1"              TO   WRK-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*作業区分
     MOVE   TO0-F21          TO   WRK-F22.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
 FURI-31-EXIT.
     EXIT.
****************************************************************
*    振替データ（３２：作業実績）                              *
****************************************************************
 FURI-32-SEC                 SECTION.
*部門コード取得
     MOVE   TO0-F01          TO   WRK-F01.
*伝票区分
     MOVE   TO0-F02          TO   WRK-F02.
*伝票■
     MOVE   TO0-F03          TO   WRK-F03.
*行■
     MOVE   TO0-F04          TO   WRK-F04.
*赤黒区分
     MOVE   TO0-F05          TO   WRK-F05.
*振替元／振替先
     MOVE   TO0-F06          TO   WRK-F06.
*発注日
     MOVE   TO0-F08          TO   WRK-F07.
*納品日
     MOVE   TO0-F09          TO   WRK-F08.
*出荷日
     MOVE   TO0-F10          TO   WRK-F09.
*商品ＣＤ
     MOVE   TO0-F11          TO   WRK-F10.
*品単ＣＤ
     MOVE   TO0-F12          TO   WRK-F11.
*単価区分／数量／単価／金額
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*入庫／出庫／発注区分－場所
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## 数量ﾏｲﾅｽ時に区分を反対にする
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*作業区分
     MOVE   TO0-F21          TO   WRK-F22.
*マスタ区分
     MOVE   "3"              TO   WRK-F20.
 FURI-32-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              TOKU  HON  FUK  SEN  HOK  OSA  OKA
                        BUTOKMF1 BUTOKMF2  SHO  SKY.
     DISPLAY  "TOKU       (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "HON      (ﾎﾝｼｬ) = "  HON-CNT       UPON   CONS.
     DISPLAY  "FUK      (ﾌｸｵｶ) = "  FUK-CNT       UPON   CONS.
     DISPLAY  "SEN     (ｾﾝﾀﾞｲ) = "  SEN-CNT       UPON   CONS.
     DISPLAY  "OKA      (ｵｶﾔﾏ) = "  OKA-CNT       UPON   CONS.
     DISPLAY  "HOK   (ﾎｯｶｲﾄﾞｳ) = "  HOK-CNT       UPON   CONS.
     DISPLAY  "OSA      (ｵｵｻｶ) = "  OSA-CNT       UPON   CONS.
     DISPLAY  "SKIP  (ﾍﾞﾂﾌﾞｼｮ) = "  SKIP-CNT      UPON   CONS.
     DISPLAY  "ｽﾄｯｸNO.(ﾎﾝｼｬ)   = "  SHO-CNT       UPON   CONS.
     DISPLAY  "ｽﾄｯｸNO.(ｷｭｳｼｭｳ) = "  SKY-CNT       UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
