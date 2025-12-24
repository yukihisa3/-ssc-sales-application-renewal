# SSE8301B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE8301B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ホーマックＷｅｂ－ＥＤＩ請求　　　*
*    モジュール名　　　　：　請求データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　06/10/12                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより　　　　　　　*
*　　　　　　　　　　　　：　送信用ファイルを作成する          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE8301B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/10/12.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル>>-*
     SELECT   SETGK83   ASSIGN         DA-01-VI-SETGK832
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F03
                                       SEI-F04
                                       SEI-F05
                        STATUS         SEI-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         TEN-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01
                                       JYO-F02
                        STATUS         JYO-ST.
*----<< 配信データ >>--*
     SELECT   SNDSEIKY  ASSIGN         DA-01-S-SNDSEIKY
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SND-ST.
*----<< 件数データ >>--*
     SELECT   HACKENDT  ASSIGN         DA-01-S-HACKENDT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEN-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル>>-*
 FD  SETGK83            LABEL RECORD   IS   STANDARD.
     COPY     SETGK83   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 配信データ >>--*
 FD  SNDSEIKY              LABEL     RECORD   IS   STANDARD.
 01  SND-REC.
     03  FILLER                   PIC  X(1026).
*
****<< 発注集計データ >>***********************************
 FD  HACKENDT
              BLOCK CONTAINS  40  RECORDS.
     COPY     HACKENDT OF        XFDLIB
              JOINING   KEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*ヘッダ情報格納領域
     COPY   HKSEIHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*明細　情報格納領域
     COPY   HKSEIMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
*テイル情報格納領域
     COPY   HKSEITAL OF XFDLIB  JOINING   TAL  AS   PREFIX.
 01  FLAGS.
     03  END-FLG           PIC  X(03)    VALUE  SPACE.
     03  HJYOKEN-INV-FLG   PIC  X(03)    VALUE  SPACE.
     03  HEAD-FLG          PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  RD-CNT            PIC  9(06).
     03  HED-CNT           PIC  9(06).
     03  MEI-CNT           PIC  9(06).
     03  TAL-CNT           PIC  9(06).
     03  DEN-CNT           PIC  9(06).
 01  WK-GOKEI              PIC S9(10)   VALUE  ZERO.
 01  WK-SYOHIZEI           PIC S9(10)   VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST                PIC  X(02).
 01  TEN-ST                PIC  X(02).
 01  JYO-ST                PIC  X(02).
 01  SND-ST                PIC  X(02).
 01  KEN-ST                PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN           PIC  9(05)     VALUE  ZERO.
     03  KEI-KIN           PIC S9(09)     VALUE  ZERO.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-AREA.
     03  WK-TORICD         PIC  9(08)     VALUE  ZERO.
     03  WK-ZEI            PIC  9(01)V9(02) VALUE  ZERO.
     03  WK-SEIKYUNO       PIC  9(09)     VALUE  ZERO.
     03  WK-SEIKYUNOS      PIC  9(09)     VALUE  ZERO.
     03  WK-SEIKYUNOE      PIC  9(09)     VALUE  ZERO.
     03  WK-SIMEBI         PIC  9(08)     VALUE  ZERO.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE              PIC  9(06).
 01  FILLER                REDEFINES      SYS-DATE.
     03  SYS-YY            PIC  9(02).
     03  SYS-MM            PIC  9(02).
     03  SYS-DD            PIC  9(02).
 01  SYS-TIME              PIC  9(08).
 01  FILLER                REDEFINES      SYS-TIME.
     03  SYS-HH            PIC  9(02).
     03  SYS-MN            PIC  9(02).
     03  SYS-SS            PIC  9(02).
     03  SYS-MS            PIC  9(02).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 SETGK83-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK83.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE4610B SETGK83 ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE4610B HTENMS ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE4610B HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 配信データ >>--*
 SNDSEIKY-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SNDSEIKY.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE4610B SNDSEIKY  ERROR " SND-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 件数データ >>--*
 HACKENDT-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HACKENDT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE4610B HACKENDT  ERROR " KEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN  UNTIL  END-FLG  =  "END".
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE4610B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SETGK83.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    SNDSEIKY.
     OPEN     OUTPUT    HACKENDT.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         GOKEI-AREA.
     INITIALIZE         WK-AREA.
*
     OPEN  INPUT  HJYOKEN.
*条件Ｆ（消費税率）
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZEI"          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG = "INV"
              DISPLAY   "HJYOKEN INV KEY=99 ZEI"  UPON STAT
              STOP  RUN
     ELSE
              MOVE  JYO-F04  TO   WK-ZEI
     END-IF.
*条件Ｆ（請求書番号）
     MOVE     "34"           TO   JYO-F01.
     MOVE     "HORMAC"       TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG = "INV"
              DISPLAY   "HJYOKEN INV KEY=34 HORMAC"  UPON STAT
              STOP  RUN
     ELSE
              MOVE  JYO-F04   TO  WK-SEIKYUNO
              MOVE  JYO-F05   TO  WK-SEIKYUNOS
              MOVE  JYO-F06   TO  WK-SEIKYUNOE
     END-IF.
*
     CLOSE   HJYOKEN.
*
     PERFORM  SEI-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*取引先ＣＤブレイクチェック
     IF   SEI-F01  NOT =  WK-TORICD
*         ヘッダ行出力判定
          IF  HEAD-FLG  = 1
              PERFORM  TAIL-WT-SEC
          END-IF
*         ヘッダ行出力
          PERFORM HEAD-WT-SEC
          MOVE   ZERO             TO   WK-GOKEI   DEN-CNT
          MOVE   SEI-F01          TO   WK-TORICD
          MOVE   1                TO   HEAD-FLG
     END-IF.
*    明細行出力
     PERFORM MEISAI-WT-SEC
*
     PERFORM  SEI-READ.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     IF  DEN-CNT   >  ZERO
         PERFORM  TAIL-WT-SEC
     END-IF.
*請求番号再更新
     OPEN  I-O    HJYOKEN.
     MOVE     "34"           TO   JYO-F01.
     MOVE     "HORMAC"       TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG = "INV"
              DISPLAY   "HJYOKEN INV KEY=34 HORMAC"  UPON STAT
              STOP  RUN
     ELSE
              MOVE   WK-SEIKYUNO  TO  JYO-F04
              REWRITE  JYO-REC
     END-IF.
*    ファイルクローズ
     CLOSE    SETGK83.
     CLOSE    HTENMS HJYOKEN.
     CLOSE    SNDSEIKY.
     CLOSE    HACKENDT.
*
     DISPLAY "+++ ｾｲｷｭｳﾃﾞｰﾀ INPUT =" RD-CNT  " +++" UPON CONS.
     DISPLAY "+++ HEDﾃﾞｰﾀ ｹﾝｽｳ    =" HED-CNT " +++" UPON CONS.
     DISPLAY "+++ MEIﾃﾞｰﾀ ｹﾝｽｳ    =" MEI-CNT " +++" UPON CONS.
     DISPLAY "+++ TALﾃﾞｰﾀ ｹﾝｽｳ    =" TAL-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8301B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
     READ     SETGK83   AT   END
              MOVE     "END"      TO   END-FLG
              NOT  AT  END
              ADD       1         TO   RD-CNT
     END-READ.
*
 SEI-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
****************************************************************
 JYO-RD-RTN           SECTION.
*
     READ     HJYOKEN
        INVALID      MOVE      "INV"     TO   HJYOKEN-INV-FLG
        NOT  INVALID MOVE      SPACE     TO   HJYOKEN-INV-FLG
     END-READ.
*
 JYO-RD-EXIT.
     EXIT.
****************************************************************
*　　　　ヘッダー出力
****************************************************************
 HEAD-WT-SEC            SECTION.
*    初期化
     MOVE     SPACE        TO     HED-REC.
     INITIALIZE                   HED-REC.
*    タグ
     MOVE "HD"             TO     HED-F01.
*    メッセージＩＤ
     MOVE "REMADV"         TO     HED-F02.
*    請求書番号
     ADD   1               TO     WK-SEIKYUNO.
     IF    WK-SEIKYUNO  >  WK-SEIKYUNOE
           MOVE WK-SEIKYUNOS TO WK-SEIKYUNO
     END-IF.
     MOVE  WK-SEIKYUNO     TO     HED-F03.
*    請求締日
     MOVE  "3"             TO     LINK-IN-KBN.
     MOVE  SEI-F02         TO     LINK-IN-YMD6.
     MOVE  ZERO            TO     LINK-IN-YMD8.
     MOVE  ZERO            TO     LINK-OUT-RET.
     MOVE  ZERO            TO     LINK-OUT-YMD.
     CALL  "SKYDTCKB"      USING  LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD    TO     HED-F04  WK-SIMEBI.
*    請求区分
     MOVE  01              TO     HED-F05.
*    発注企業名称カナ
     MOVE X"28"            TO     HED-A041.
     MOVE "ﾎｰﾏｯｸ"          TO     HED-F06.
     MOVE X"29"            TO     HED-A042.
*    発注企業名称漢字
     MOVE NC"ホーマック"   TO     HED-F07.
*    発注企業コード
     MOVE "E0__"        TO     HED-F08.
*    ＧＬＮ発注企業コード
     MOVE 4949754000000 TO     HED-F09.
*    請求先事業部コード
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273  MOVE  02   TO  HED-F10
         WHEN  882  WHEN  14272  MOVE  03   TO  HED-F10
         WHEN  880  WHEN  1427   MOVE  04   TO  HED-F10
     END-EVALUATE.
*    請求先事業部名カナ
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273
         MOVE  "ﾎｯｶｲﾄﾞｳ ｼﾞｷﾞｮｳﾌﾞ"           TO  HED-F11
         WHEN  882  WHEN  14272
         MOVE  "ﾄｳﾎｸ ｼﾞｷﾞｮｳﾌﾞ "             TO  HED-F11
         WHEN  880  WHEN  1427
         MOVE  "ｶﾝﾄｳ ｼﾞｷﾞｮｳﾌﾞ"              TO  HED-F11
     END-EVALUATE.
*    請求先事業部名漢字
     MOVE X"28"         TO     HED-A081.
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273
         MOVE  NC"北海道事業部"             TO  HED-F12
         WHEN  882  WHEN  14272
         MOVE  NC"東北事業部"               TO  HED-F12
         WHEN  880  WHEN  1427
         MOVE  NC"関東事業部"               TO  HED-F12
     END-EVALUATE.
     MOVE X"29"         TO     HED-A082.
*    取引先コード
     EVALUATE  SEI-F01
         WHEN  883   MOVE  880   TO  HED-F13
         WHEN  882   MOVE  880   TO  HED-F13
         WHEN  880   MOVE  880   TO  HED-F13
         WHEN  14273 MOVE  1427  TO  HED-F13
         WHEN  14272 MOVE  1427  TO  HED-F13
         WHEN  1427  MOVE  1427  TO  HED-F13
     END-EVALUATE.
*    取引先制御コード
     MOVE ZERO          TO     HED-F14.
*    取引先名カナ
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273
         MOVE  "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ ﾎｯｶｲﾄﾞｳｼﾃﾝ ﾎｰﾑｶﾞｰﾃﾞﾝﾌﾞ"    TO HED-F15
         WHEN  882  WHEN  14272
         MOVE  "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ ｾﾝﾀﾞｲｴｲｷﾞｮｳｼｮ ﾎｰﾑｶﾞｰﾃﾞﾝﾌﾞ" TO HED-F15
         WHEN  880  WHEN  1427
         MOVE  "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ ﾋｶﾞｼﾆﾎﾝｼﾃﾝ ﾎｰﾑｶﾞｰﾃﾞﾝﾌﾞ"    TO HED-F15
     END-EVALUATE.
*    取引先名漢字
     MOVE X"28"         TO     HED-A101.
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273
         MOVE  NC"_サカタのタネ　北海道支店"  TO HED-F16
         WHEN  882  WHEN  14272
         MOVE  NC"_サカタのタネ　仙台営業所"  TO HED-F16
         WHEN  880  WHEN  1427
         MOVE  NC"_サカタのタネ　関東事業部"  TO HED-F16
     END-EVALUATE.
     MOVE X"29"         TO     HED-A102.
     MOVE "0"           TO     HED-A11(425:1).
*
     MOVE   SPACE       TO     SND-REC.
     INITIALIZE                SND-REC.
     MOVE   HED-REC     TO     SND-REC.
     WRITE  SND-REC.
*
     ADD    1           TO     HED-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*　　　　明細出力
****************************************************************
 MEISAI-WT-SEC          SECTION.
*    初期化
     MOVE     SPACE        TO     MEI-REC.
     INITIALIZE                   MEI-REC.
*    タグ
     MOVE "DT"             TO     MEI-F01.
*    発注伝票番号
     MOVE  SEI-F05         TO     MEI-F02.
*    請求額
     IF    SEI-F06  <  ZERO
           MOVE   "-"      TO     MEI-F031
     ELSE
           MOVE   "0"      TO     MEI-F031
     END-IF.
     MOVE  SEI-F06         TO     MEI-F032.
     ADD   SEI-F06         TO     WK-GOKEI.
*    発注日
     MOVE  SEI-F13         TO     MEI-F04.
     IF SEI-F13  NOT  NUMERIC
           MOVE    ZERO    TO     MEI-F04
     END-IF.
*    納品日
     MOVE  "3"             TO     LINK-IN-KBN.
     MOVE  SEI-F10         TO     LINK-IN-YMD6.
     MOVE  ZERO            TO     LINK-IN-YMD8.
     MOVE  ZERO            TO     LINK-OUT-RET.
     MOVE  ZERO            TO     LINK-OUT-YMD.
     CALL  "SKYDTCKB"      USING  LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD    TO     MEI-F05  MEI-F06.
*    請求先事業部コード
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273  MOVE  02   TO  MEI-F07
         WHEN  882  WHEN  14272  MOVE  03   TO  MEI-F07
         WHEN  880  WHEN  1427   MOVE  04   TO  MEI-F07
     END-EVALUATE.
*    納品業部名カナ
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273
         MOVE  "ﾎｯｶｲﾄﾞｳ ｼﾞｷﾞｮｳﾌﾞ"           TO  MEI-F08
         WHEN  882  WHEN  14272
         MOVE  "ﾄｳﾎｸ ｼﾞｷﾞｮｳﾌﾞ "             TO  MEI-F08
         WHEN  880  WHEN  1427
         MOVE  "ｶﾝﾄｳ ｼﾞｷﾞｮｳﾌﾞ"              TO  MEI-F08
     END-EVALUATE.
*    納品事業部名漢字
     MOVE X"28"         TO     MEI-A031.
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273
         MOVE  NC"北海道事業部"             TO  MEI-F09
         WHEN  882  WHEN  14272
         MOVE  NC"東北事業部"               TO  MEI-F09
         WHEN  880  WHEN  1427
         MOVE  NC"関東事業部"               TO  MEI-F09
     END-EVALUATE.
     MOVE X"29"         TO     MEI-A032.
*    伝票種別
     EVALUATE  SEI-F07
         WHEN  40    MOVE  01    TO  MEI-F10
         WHEN  41    MOVE  02    TO  MEI-F10
         WHEN  42    MOVE  03    TO  MEI-F10
     END-EVALUATE.
*    納品先コード
     MOVE SEI-F03       TO     MEI-F11.
*    納品先名カナ
     MOVE      SEI-F01      TO    TEN-F52.
     MOVE      SEI-F03      TO    TEN-F011.
     READ   HTENMS
            INVALID
            MOVE   SPACE    TO    MEI-F12
            MOVE   X"28"    TO    MEI-A051
            MOVE ALL NC"　" TO    MEI-F13
            MOVE   X"29"    TO    MEI-A052
            NOT  INVALID
            MOVE   TEN-F04  TO    MEI-F12
            MOVE   X"28"    TO    MEI-A051
            MOVE   TEN-F03  TO    MEI-F13
            MOVE   X"29"    TO    MEI-A052
     END-READ.
     MOVE "0"           TO     MEI-A07(6:1).
*
     MOVE   SPACE       TO     SND-REC.
     INITIALIZE                SND-REC.
     MOVE   MEI-REC     TO     SND-REC.
     WRITE  SND-REC.
*
     ADD    1           TO     MEI-CNT  DEN-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　合計行出力
****************************************************************
 TAIL-WT-SEC            SECTION.
*    初期化
     MOVE     SPACE        TO     TAL-REC  KEN-REC.
     INITIALIZE                   TAL-REC  KEN-REC.
*    請求締日
     MOVE WK-SIMEBI        TO     KEN-F01.
*    取引先コード
     MOVE WK-TORICD        TO     KEN-F02.
*    タグ
     MOVE "TR"             TO     TAL-F01.
*    伝票枚数
     MOVE  DEN-CNT         TO     TAL-F02   KEN-F03.
*    請求合計額
     IF    WK-GOKEI  <  ZERO
           MOVE   "-"      TO     TAL-F031
     ELSE
           MOVE   "0"      TO     TAL-F031
     END-IF.
     MOVE  WK-GOKEI        TO     TAL-F032  KEN-F04.
*    消費税額計算
     COMPUTE  WK-SYOHIZEI  =  WK-GOKEI  *  ( WK-ZEI  -  1).
     IF    WK-SYOHIZEI  <  ZERO
           MOVE   "-"      TO     TAL-F041
     ELSE
           MOVE   "0"      TO     TAL-F041
     END-IF.
     MOVE  WK-SYOHIZEI     TO     TAL-F042  KEN-F05.
     MOVE "0"           TO     TAL-A04(50:1).
*
     MOVE   SPACE       TO     SND-REC.
     INITIALIZE                SND-REC.
     MOVE   TAL-REC     TO     SND-REC.
     WRITE  SND-REC.
     WRITE  KEN-REC.
*
     ADD    1           TO     TAL-CNT.
*
 TAIL-WT-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
