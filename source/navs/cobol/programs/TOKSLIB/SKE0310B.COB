****************************************************************
*    ※取引先ＣＤ＝０７６０専用
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*　　業務名　　　　　　　：　出荷検品システム
*　　モジュール名　　　　：　送信ＤＴ作成
*　　作成日／更新日　　　：　2001/05/14
*　　作成者／更新者　　　：　NAV
*　　処理概要　　　　　　：　出荷情報Ｆ・梱包ラベルＦより、ケー
*　　　　　　　　　　　　　　ヨー送信Ｆを作成する。　　　　　
*    更新履歴            ：
*      2011/10/06 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE0310B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/05/14.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  出荷情報データ  >>---*
     SELECT   RUISYUF   ASSIGN         DA-01-VI-RUISYUL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RUI-F11
                                       RUI-F04
                                       RUI-F03
                                       RUI-F02
                                       RUI-F05
                                       RUI-F06
                                       RUI-F07
                        STATUS         RUI-ST.
*---<<  梱包ラベルデータ  >>---*
     SELECT   RUIKONF   ASSIGN         DA-01-VI-RUIKONL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KON-F05
                                       KON-F08
                                       KON-F03
                                       KON-F02
                                       KON-F04
                        STATUS         KON-ST.
*----<< 伝票データ >>--*
     SELECT   SHTDENF   ASSIGN    TO   DA-01-VI-SHTDENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DEN-F01
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                  *> 2011/10/06,S  S.I/NAV
                                       DEN-F07
                                       DEN-F112
                                  *> 2011/10/06,E  S.I/NAV
                                       DEN-F03
                        FILE  STATUS   DEN-ST.
*----<< ケーヨー送信Ｆ >>--*
     SELECT   SNDKENF   ASSIGN    TO   DA-01-S-SNDKENF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SND-ST.
*----<< 倉庫検収Ｆ >>--*
     SELECT   KENSOKF   ASSIGN    TO   DA-01-VI-KENSOKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                                       SOK-F02
                                       SOK-F03
                        FILE  STATUS   SOK-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷情報データ >>--*
 FD  RUISYUF            LABEL RECORD   IS   STANDARD.
     COPY        RUISYUF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*----<< 梱包ラベルデータ >>--*
 FD  RUIKONF            LABEL RECORD   IS   STANDARD.
     COPY        RUIKONF     OF      XFDLIB
                 JOINING     KON     PREFIX.
*----<< 伝票データ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< ケーヨー送信データ  >>--*
 FD  SNDKENF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       31   RECORDS.
 01  SND-REC.
     03  ONL0-REC                 PIC  X(128).
     03  ONLR1-REC  REDEFINES     ONL0-REC.
       05  ONL1-REC.
         07  ONL1-A1              PIC  X(01).
         07  ONL1-A2              PIC  X(02).
         07  ONL1-A3              PIC  9(05).
         07  ONL1-A4              PIC  9(03).
         07  ONL1-A5              PIC  X(08).
         07  ONL1-A6              PIC  X(01).
         07  ONL1-A7              PIC  9(05).
         07  ONL1-A8              PIC  X(08).
         07  ONL1-A9              PIC  X(03).
         07  ONL1-A10             PIC  X(04).
     03  ONLR2-REC  REDEFINES     ONL0-REC.
       05  ONL2-REC.
         07  ONL2-A1              PIC  X(01).
         07  ONL2-A2              PIC  X(02).
         07  ONL2-A3              PIC  9(05).
         07  ONL2-A4              OCCURS    6.
           09  ONL2-A41           PIC  X(18).
           09  ONL2-A42           PIC  X(02).
     03  ONLR3-REC  REDEFINES     ONL0-REC.
       05  ONL3-REC.
         07  ONL3-A1              PIC  X(01).
         07  ONL3-A2              PIC  X(02).
         07  ONL3-A3              PIC  9(05).
         07  ONL3-A4              PIC  X(03).
         07  ONL3-A5              PIC  X(04).
*********07  ONL3-A6              PIC  X(02).
         07  ONL3-A7              PIC  X(06).
         07  ONL3-A8              PIC  X(09).
         07  ONL3-A9              PIC  9(02).
         07  ONL3-A10             PIC  X(07).
         07  ONL3-A11             PIC  X(13).
         07  ONL3-A12             PIC  9(05)V9(01).
         07  ONL3-A13             PIC  9(05)V9(01).
     03  ONLR4-REC  REDEFINES     ONL0-REC.
       05  ONL4-REC.
         07  ONL4-A1              PIC  X(01).
         07  ONL4-A2              PIC  X(02).
         07  ONL4-A3              PIC  9(05).
         07  ONL4-A4              PIC  X(07).
         07  ONL4-A5              PIC  X(06).
         07  ONL4-A6              PIC  X(08).
         07  ONL4-A7              PIC  X(08).
         07  ONL4-A8              PIC  X(08).
         07  ONL4-A9              PIC  X(83).
*----<< 倉庫検収Ｆ >>--*
 FD  KENSOKF            LABEL     RECORD   IS   STANDARD.
     COPY     KENSOKF   OF        XFDLIB
              JOINING   SOK       PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
     03  END-FLG2       PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  ONL1-CNT       PIC  9(06).
     03  ONL2-CNT       PIC  9(06).
     03  ONL3-CNT       PIC  9(06).
     03  ONL4-CNT       PIC  9(06).
     03  DEN-CNT        PIC  9(06).
     03  SOK-CNT        PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-RENBAN          PIC  9(05)     VALUE  ZERO.
 01  IX                 PIC  9(01)     VALUE  ZERO.
 01  KENSOKF-INV-FLG    PIC  X(03)     VALUE  SPACE.
 01  WK-CHK-AREA.
     03  NEW-TEN        PIC  9(05)     VALUE  ZERO.
     03  OLD-TEN        PIC  9(05)     VALUE  ZERO.
     03  NEW-KEN        PIC  9(08)     VALUE  ZERO.
     03  OLD-KEN        PIC  9(08)     VALUE  ZERO.
     03  NEW-TOK        PIC  9(04)     VALUE  ZERO.
     03  OLD-TOK        PIC  9(04)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RUI-ST             PIC  X(02).
 01  KON-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  SND-ST             PIC  X(02).
 01  SOK-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-SYS-DATE        PIC  9(08).
 01  WK-SYS-TIME        PIC  9(06).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  WK-RUI.
     03  WK-RUI-F04     PIC  9(04)     VALUE  ZERO.
     03  WK-RUI-F03     PIC  9(08)     VALUE  ZERO.
     03  WK-RUI-F02     PIC  9(05)     VALUE  ZERO.
 01  WK-HED-TOKCD.
     03  WK-HED-01      PIC  X(02)     VALUE  SPACE.
     03  WK-HED-02      PIC  X(04)     VALUE  SPACE.
     03  WK-HED-03      PIC  X(02)     VALUE  SPACE.
 01  LINK-AREA2.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷情報データ >>--*
 RUI-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUISYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0310B RUISYUF    ERROR " RUI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 梱包ラベルデータ >>--*
 KON-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUIKONF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0310B RUIKONF    ERROR " KON-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 伝票データ >>--*
 DENL-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0310B SHTDENL1   ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ケーヨー送信データ >>--*
 SND-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SNDKENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0310B SNDKENF ERROR " SND-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 倉庫検収Ｆ >>--*
 SOK-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KENSOKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0310B KENSOKL1   ERROR " SOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   WK-SYS-DATE.
     MOVE     SYS-TIME(1:6)  TO   WK-SYS-TIME.
*ファイルＯＰＥＮ
     OPEN     I-O       RUISYUF RUIKONF SHTDENF KENSOKF.
     OPEN     OUTPUT    SNDKENF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     MOVE     ZERO      TO      WK-RENBAN.
*出荷情報データ初期読込み
     PERFORM  RUISYUF-READ-SEC.
     IF   END-FLG  =  "END"
          DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
     ELSE
**********PERFORM  FILE-HEAD-SEC
          MOVE     RUI-F04   TO   WK-RUI-F04
          MOVE     RUI-F03   TO   WK-RUI-F03
          MOVE     RUI-F02   TO   WK-RUI-F02
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    配送店ヘッダ出力
     IF  NEW-TEN  NOT =  OLD-TEN
     OR  NEW-KEN  NOT =  OLD-KEN
     OR  NEW-TOK  NOT =  OLD-TOK
*        梱包ラベルデータ出力
*********IF   WK-RENBAN  >  ZERO
         IF   WK-RENBAN  >  1
**************DISPLAY "WK-RUI-F01 = " WK-RUI-F01 UPON CONS
**************DISPLAY "WK-RUI-F03 = " WK-RUI-F03 UPON CONS
**************DISPLAY "WK-RUI-F02 = " WK-RUI-F02 UPON CONS
              MOVE     "END"    TO  END-FLG2
              MOVE      6       TO  IX
              PERFORM  RUIKONF-ST-SEC
              PERFORM  RUIKONF-SET-SEC  UNTIL  END-FLG2  =  "END"
              IF     ( IX  >  ZERO )
              AND    ( IX  <  6    )
                       WRITE   SND-REC
                       ADD     1          TO   ONL2-CNT
              END-IF
         END-IF
         IF  NEW-TOK  NOT =  OLD-TOK
             PERFORM  FILE-HEAD-SEC
         END-IF
         PERFORM  RUISYUF-HEAD-SEC
         MOVE     NEW-TEN  TO  OLD-TEN
         MOVE     NEW-KEN  TO  OLD-KEN
         MOVE     NEW-TOK  TO  OLD-TOK
         MOVE     RUI-F04  TO  WK-RUI-F04
         MOVE     RUI-F03  TO  WK-RUI-F03
         MOVE     RUI-F02  TO  WK-RUI-F02
     END-IF.
*    出荷情報出力
     PERFORM  RUISYUF-SET-SEC.
*    送信済ＦＬＧセット
     PERFORM  SHTDENF-READ-SEC.
*    出荷情報読込み
     PERFORM  RUISYUF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*    最終梱包データ出力
     MOVE     "END"    TO  END-FLG2.
     MOVE      6       TO  IX.
     PERFORM  RUIKONF-ST-SEC.
     PERFORM  RUIKONF-SET-SEC  UNTIL  END-FLG2  =  "END".
     IF     ( IX  >  ZERO )
     AND    ( IX  <  6    )
              WRITE   SND-REC
              ADD     1          TO   ONL2-CNT
     END-IF.
*
     CLOSE    RUISYUF   RUIKONF   SHTDENF  SNDKENF  KENSOKF.
*
     DISPLAY  "## ｼｭｯｶｼﾞｮｳﾎｳ   IN => " IN-CNT    UPON CONS.
     DISPLAY  "## Fﾍｯﾀﾞｼﾞｮｳﾎｳ OUT => " ONL4-CNT  UPON CONS.
     DISPLAY  "## ﾍｯﾀﾞｼﾞｮｳﾎｳ  OUT => " ONL1-CNT  UPON CONS.
     DISPLAY  "## ｺﾝﾎﾟｳｼﾞｮｳﾎｳ OUT => " ONL2-CNT  UPON CONS.
     DISPLAY  "## ｼｭｯｶｼﾞｮｳﾎｳ  OUT => " ONL3-CNT  UPON CONS.
     DISPLAY  "## ｳﾘｱｹﾞﾌｧｲﾙ   SET => " DEN-CNT   UPON CONS.
     DISPLAY  "## ｹﾝｼｭｳﾃﾞｰﾀ   SET => " SOK-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0310B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUISYUF-READ-SEC       SECTION.
*    売上伝票ファイル参照
     READ     RUISYUF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   RUISYUF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
     IF       IN-CNT(4:3) = "000" OR "500"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*    送信ＦＬＧチェック
     IF       RUI-F11  =  1
              MOVE  "END"    TO   END-FLG
              GO             TO   RUISYUF-READ-EXIT
     END-IF.
*    取引先制限
*取引先ＣＤが”０７６０”の時のみ対象
     IF       RUI-F04   =   760
              CONTINUE
     ELSE
              GO             TO   RUISYUF-READ-SEC
     END-IF.
*ブレイクキーセット
     MOVE     RUI-F02        TO   NEW-TEN.
     MOVE     RUI-F03        TO   NEW-KEN.
     MOVE     RUI-F04        TO   NEW-TOK.
*
 RUISYUF-READ-EXIT.
     EXIT.
****************************************************************
*　　梱包ラベルデータスタート　　　　　　　　　　　　　　　　　*
****************************************************************
 RUIKONF-ST-SEC         SECTION.
*    梱包ラベルデータスタート
     MOVE     SPACE          TO   END-FLG2.
     MOVE     ZERO           TO   KON-F05.
     MOVE     WK-RUI-F04     TO   KON-F08.
     MOVE     WK-RUI-F03     TO   KON-F03.
     MOVE     WK-RUI-F02     TO   KON-F02.
     MOVE     SPACE          TO   KON-F04.
     START    RUIKONF  KEY  IS  >=  KON-F05 KON-F08 KON-F03
                                    KON-F02 KON-F04
              INVALID
              MOVE   "END"   TO   END-FLG2
              NOT  INVALID
              PERFORM  RUIKONF-READ-SEC
     END-START.
*****DISPLAY "AAAAA"  UPON CONS.
*
 RUIKONF-ST-EXIT.
     EXIT.
****************************************************************
*　　梱包ラベルデータ読込み　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUIKONF-READ-SEC       SECTION.
*
     READ   RUIKONF  NEXT  AT  END
            MOVE   "END"     TO       END-FLG2
            GO               TO       RUIKONF-READ-EXIT
     END-READ.
*店舗チェック
     IF     WK-RUI-F02  NOT =  KON-F02
     OR     WK-RUI-F03  NOT =  KON-F03
     OR     WK-RUI-F04  NOT =  KON-F08
            MOVE   "END"     TO       END-FLG2
     END-IF.
*****DISPLAY "BBBBB"  UPON CONS.
*
 RUIKONF-READ-EXIT.
     EXIT.
****************************************************************
*　　売上伝票Ｆ　送信済ＦＬＧ更新　　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC       SECTION.
*    売上伝票Ｆ読み込み
     MOVE     RUI-F01        TO   DEN-F01.
     MOVE     RUI-F06        TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     MOVE     RUI-F07        TO   DEN-F03.
* 2011/10/06,S  S.I/NAV
     MOVE  RUI-F02          TO  DEN-F07.  *> 店舗CD
     MOVE  RUI-F05          TO  DEN-F112. *> 納品日
* 2011/10/06,E  S.I/NAV
     READ     SHTDENF   INVALID
**************DISPLAY "ｳﾘｱｹﾞDT INVALID KEY = " RUI-F06 " : "
***********************RUI-F07 UPON CONS
              GO             TO       SHTDENF-READ-EXIT
     END-READ.
*
     MOVE     1              TO       DEN-F27A.
*
     PERFORM  KENSOKF-SET-SEC.
*
     REWRITE  DEN-REC.
     ADD      1              TO       DEN-CNT.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　ファイルヘッダレコード作成　　　　　　　　　　　　　　　　*
****************************************************************
 FILE-HEAD-SEC          SECTION.
*レコード初期化
     MOVE     SPACE          TO   SND-REC.
     INITIALIZE                   SND-REC.
*項目セット
*＜レコード区分＞
     MOVE     "A"            TO   ONL4-A1.
*＜データ種別＞
     MOVE     "21"           TO   ONL4-A2.
*＜データ連番＞
     ADD      1              TO   WK-RENBAN.
     MOVE     WK-RENBAN      TO   ONL4-A3.
*＜データ日付＞
     MOVE     SYS-DATE       TO   ONL4-A5.
*＜データ送信元＞
*****MOVE     "00017300"     TO   ONL4-A6.
     MOVE     "00"           TO   WK-HED-01.
     MOVE     RUI-F04        TO   WK-HED-02.
     MOVE     "00"           TO   WK-HED-03.
     MOVE     WK-HED-TOKCD   TO   ONL4-A6.
*＜最終送信元＞
     MOVE     "499900  "     TO   ONL4-A7.
*＜直接送信宛先＞
     MOVE     "00001500"     TO   ONL4-A8.
*＜ファイルヘッダコード出力＞
     WRITE    SND-REC.
     ADD      1              TO   ONL4-CNT.
*
 FILE-HEAD-EXIT.
     EXIT.
****************************************************************
*　　倉庫検収データ作成　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KENSOKF-SET-SEC        SECTION.
*    倉庫検収データ読込み
     MOVE     DEN-F48        TO   SOK-F01.
     MOVE     RUI-F06        TO   SOK-F02.
     MOVE     RUI-F07        TO   SOK-F03.
     READ     KENSOKF   INVALID
              MOVE    "INV"  TO   KENSOKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE  TO   KENSOKF-INV-FLG
     END-READ.
*
     IF       KENSOKF-INV-FLG = SPACE
              GO             TO   KENSOKF-SET-EXIT
     END-IF.
*
     MOVE     SPACE          TO   SOK-REC.
     INITIALIZE                   SOK-REC.
*
     MOVE     DEN-F48        TO   SOK-F01.
     MOVE     RUI-F06        TO   SOK-F02.
     MOVE     RUI-F07        TO   SOK-F03.
     MOVE     RUI-F01        TO   SOK-F04.
*
     WRITE    SOK-REC.
     ADD      1              TO   SOK-CNT.
*
 KENSOKF-SET-EXIT.
     EXIT.
****************************************************************
*　　配送店レコードセット　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUISYUF-HEAD-SEC       SECTION.
*レコード初期化
     MOVE     SPACE          TO   SND-REC.
     INITIALIZE                   SND-REC.
*項目セット
*＜レコード区分＞
     MOVE     "B"            TO   ONL1-A1.
*＜データ種別＞
     MOVE     "21"           TO   ONL1-A2.
*＜データ連番＞
     ADD      1              TO   WK-RENBAN.
     MOVE     WK-RENBAN      TO   ONL1-A3.
*＜配送予定店舗＞
     MOVE     RUI-F02        TO   ONL1-A4.
*＜積込検品実施日＞
     MOVE     RUI-F03        TO   ONL1-A5.
*＜ルート区分＞
     MOVE     SPACE          TO   ONL1-A6.
*＜出荷口数＞
     MOVE     ZERO           TO   ONL1-A7.
*＜ケーヨー企業ＣＤ＞
     MOVE     "499900  "     TO   ONL1-A8.
*＜店舗ＣＤ＞
     MOVE     RUI-F02(3:3)   TO   ONL1-A9.
*＜取引先ＣＤ＞
     MOVE     RUI-F04        TO   ONL1-A10.
*＜配送店ヘッダレコード出力＞
     WRITE    SND-REC.
     ADD      1              TO   ONL1-CNT.
*
 RUISYUF-HEAD-EXIT.
     EXIT.
****************************************************************
*　　梱包ラベル情報セット　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUIKONF-SET-SEC        SECTION.
*レコード初期化
*****DISPLAY "CCCCC"  UPON CONS.
     IF       IX  >=  6
              MOVE   SPACE   TO   SND-REC
              INITIALIZE          SND-REC
*             ＜レコード区分＞
              MOVE   "E"     TO   ONL2-A1
*             ＜データ種別＞
              MOVE   "21"    TO   ONL2-A2
*             ＜データ連番＞
              ADD     1      TO   WK-RENBAN
              MOVE WK-RENBAN TO   ONL2-A3
              MOVE    ZERO   TO   IX
     END-IF.
*項目セット
*＜テーブル＞
     ADD      1              TO   IX.
*＜梱包ラベル＞
     MOVE     KON-F04        TO   ONL2-A41(IX).
     IF   IX  =  6
          WRITE   SND-REC
          ADD     1          TO   ONL2-CNT
     END-IF.
*＜送信ＦＬＧ、送信日、送信時間セット＞
     MOVE    "1"             TO   KON-F05.
     MOVE     WK-SYS-DATE    TO   KON-F06.
     MOVE     WK-SYS-TIME    TO   KON-F07.
     REWRITE  KON-REC.
*梱包ラベルデータ読込み
     PERFORM  RUIKONF-READ-SEC.
*
 RUIKONF-SET-EXIT.
     EXIT.
****************************************************************
*　　出荷情報レコードセット　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUISYUF-SET-SEC        SECTION.
*レコード初期化
     MOVE     SPACE          TO   SND-REC.
     INITIALIZE                   SND-REC.
*項目セット
*＜レコード区分＞
     MOVE     "C"            TO   ONL3-A1.
*＜データ種別＞
     MOVE     "21"           TO   ONL3-A2.
*＜データ連番＞
     ADD      1              TO   WK-RENBAN.
     MOVE     WK-RENBAN      TO   ONL3-A3.
*＜店舗ＣＤ＞
     MOVE     RUI-F02(3:3)   TO   ONL3-A4.
*＜取引先ＣＤ＞
     MOVE     RUI-F04        TO   ONL3-A5.
*＜納品指定日＞
     MOVE     RUI-F05(3:6)   TO   ONL3-A7.
*＜伝票番号＞
     MOVE     RUI-F06        TO   ONL3-A8.
*＜行番号＞
     MOVE     RUI-F07        TO   ONL3-A9.
*＜商品ＣＤ＞
     MOVE    SPACE           TO   ONL3-A10.
*＜ＪＡＮＣＤ＞
     MOVE     RUI-F08        TO   ONL3-A11.
*＜発注数量＞
     MOVE     RUI-F09        TO   ONL3-A12.
*＜出荷数量＞
     MOVE     RUI-F10        TO   ONL3-A13.
*＜出荷情報出力＞
     WRITE    SND-REC.
     ADD      1              TO   ONL3-CNT.
*＜送信ＦＬＧ、送信日、送信時間＞
     MOVE    "1"             TO   RUI-F11.
     MOVE     WK-SYS-DATE    TO   RUI-F12.
     MOVE     WK-SYS-TIME    TO   RUI-F13.
     REWRITE  RUI-REC.
*
 RUISYUF-SET-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
