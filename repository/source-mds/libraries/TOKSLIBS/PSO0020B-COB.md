# PSO0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/PSO0020B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト　　ＷＥＢＥＤＩ　　*
*    業務名　　　　　　　：　ダイユーエイト　　ＷＥＢＥＤＩ　　*
*    モジュール名　　　　：　ＰＯＳ売上情報リストファイル作成  *
*    作成日／更新日　　　：　2010/07/13                        *
*    作成者／更新者　　　：　NAV ABE                           *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            をＰＯＳ売上情報ファイルより抽出。*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            PSO0020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/07/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＰＯＳ売上情報ファイル
     SELECT   DYPSURF   ASSIGN    TO        DA-01-VI-DYPSURL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       PSJ-F01   PSJ-F02
                                            PSJ-F03   PSJ-F04
                                            PSJ-F05
                        FILE      STATUS    PSJ-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*ＰＯＳ売上情報リストファイル
     SELECT   DYPSLTL1  ASSIGN    TO        DA-01-VI-DYPSLTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       PSL-F01   PSL-F02
                                            PSL-F03   PSL-F04
                        FILE      STATUS    PSL-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＰＯＳ売上情報ファイル
******************************************************************
 FD  DYPSURF
                        LABEL RECORD   IS   STANDARD.
     COPY     DYPSURF   OF        XFDLIB
              JOINING   PSJ  AS   PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    ＰＯＳ売上情報リストファイル
******************************************************************
 FD  DYPSLTL1           LABEL RECORD   IS   STANDARD.
     COPY     DYPSLTL1  OF        XFDLIB
              JOINING   PSL       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(10)     VALUE  ZERO.
     03  O-CNT1              PIC  9(10)     VALUE  ZERO.
     03  O-CNT2              PIC  9(10)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  PSLYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  DYPSURF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  INV-FLG             PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  PSJ-STATUS        PIC  X(02).
     03  PSL-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
*ブレイクキー
 01  BRK-KEY.
     03  BRK-TORICD        PIC  9(06).
     03  BRK-MISECD        PIC  9(06).
     03  BRK-JANCD         PIC  X(14).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "PSO0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "PSO0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "PSO0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
*集計領域
 01  WK-SYUKEI.
     03  WK-URISU           PIC   9(12)  VALUE  ZERO.
     03  WK-TOKURSU         PIC   9(12)  VALUE  ZERO.
     03  WK-URIKIN          PIC   9(12)  VALUE  ZERO.
     03  WK-TOKURKIN        PIC   9(12)  VALUE  ZERO.
     03  WK-HATHUMEI        PIC   N(10)  VALUE  SPACE.
*
 01  WK-KEISAN.
     03  WK-GENKA           PIC   9(09)V9(3).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SURID             PIC   9(08).
 01  PARA-EURID             PIC   9(08).
 01  PARA-STENCD            PIC   9(06).
 01  PARA-ETENCD            PIC   9(06).
 01  PARA-SSHOCD            PIC   9(13).
 01  PARA-ESHOCD            PIC   9(13).
 01  PARA-KBN               PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TORICD
                                       PARA-SURID
                                       PARA-EURID
                                       PARA-STENCD
                                       PARA-ETENCD
                                       PARA-SSHOCD
                                       PARA-ESHOCD
                                       PARA-KBN.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYPSURF.
     MOVE      "DYPSURL1"   TO   AB-FILE.
     MOVE      PSJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYPSLTL1.
     MOVE      "DYPSLTL1 "   TO   AB-FILE.
     MOVE      PSL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*    DISPLAY "TORICD = " PARA-TORICD  UPON CONS.
*    DISPLAY "SURID  = " PARA-SURID   UPON CONS.
*    DISPLAY "EURID  = " PARA-EURID   UPON CONS.
*    DISPLAY "STENCD = " PARA-STENCD  UPON CONS.
*    DISPLAY "ETENCD = " PARA-ETENCD  UPON CONS.
*    DISPLAY "SSHOCD = " PARA-SSHOCD  UPON CONS.
*    DISPLAY "ESHOCD = " PARA-ESHOCD  UPON CONS.
*    DISPLAY "KBN    = " PARA-KBN     UPON CONS.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     DYPSURF   SHOTBL1   MEIMS1.
     OPEN     OUTPUT    DYPSLTL1.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*    ＰＯＳ売上情報ファイルスタート
     MOVE     SPACE          TO   PSJ-REC.
     INITIALIZE                   PSJ-REC.
     MOVE     PARA-TORICD    TO   PSJ-F01.
     MOVE     PARA-STENCD    TO   PSJ-F02.
     MOVE     PARA-SURID     TO   PSJ-F05.
     MOVE     PARA-SSHOCD    TO   PSJ-F04.
*
     START    DYPSURF   KEY  >=   PSJ-F01   PSJ-F02
                                  PSJ-F03   PSJ-F04
                                  PSJ-F05
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＰＯＳ売上情報ファイル読込み
     PERFORM DYPSURL1-READ-SEC.
*
     MOVE      SPACE              TO  BRK-KEY.
*    帳票種別によりブレイクキーの設定を変更
     IF    PARA-KBN  =  "2"
           MOVE   PSJ-F01         TO  BRK-TORICD
           MOVE   PSJ-F02         TO  BRK-MISECD
           MOVE   PSJ-F04         TO  BRK-JANCD
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　ＰＯＳ売上情報ファイル読込み　　　　　　　　　　　　　　　*
****************************************************************
 DYPSURL1-READ-SEC   SECTION.
*
     READ     DYPSURF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DYPSURL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    終了チェック
     IF       PSJ-F01      >  PARA-TORICD
              MOVE     "END"        TO  END-FLG
              GO                    TO  DYPSURL1-READ-EXIT
     END-IF.
*    対象外店舗コード読み飛ばし
     IF     ( PSJ-F02     >=  PARA-STENCD  ) AND
            ( PSJ-F02     <=  PARA-ETENCD  )
              CONTINUE
     ELSE
              GO                    TO  DYPSURL1-READ-SEC
     END-IF.
*    対象外ＪＡＮＣＤ読み飛ばし
     IF     ( PSJ-F04     >=  PARA-SSHOCD  ) AND
            ( PSJ-F04     <=  PARA-ESHOCD  )
              CONTINUE
     ELSE
              GO                    TO  DYPSURL1-READ-SEC
     END-IF.
*    対象外売上年月日読み飛ばし
     IF     ( PSJ-F05     >=  PARA-SURID   ) AND
            ( PSJ-F05     <=  PARA-EURID   )
              CONTINUE
     ELSE
              GO                    TO  DYPSURL1-READ-SEC
     END-IF.
*
 DYPSURL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*ＰＯＳ売上情報リストデータ作成
     EVALUATE   PARA-KBN
         WHEN   "1"
                PERFORM  DYPSLTL1-WRITE1-SEC
         WHEN   "2"
                IF   PSJ-F01     NOT = BRK-TORICD  OR
                     PSJ-F02     NOT = BRK-MISECD  OR
                     PSJ-F04     NOT = BRK-JANCD
                     PERFORM  DYPSLTL1-WRITE2-SEC
                     MOVE   PSJ-F01    TO  BRK-TORICD
                     MOVE   PSJ-F02    TO  BRK-MISECD
                     MOVE   PSJ-F04    TO  BRK-JANCD
                     MOVE   PSJ-H07    TO  WK-HATHUMEI
                     MOVE   ZERO       TO  WK-SYUKEI
                     INITIALIZE            WK-SYUKEI
                 END-IF
                 ADD  PSJ-M213         TO  WK-URISU
                 ADD  PSJ-M212         TO  WK-TOKURSU
                 ADD  PSJ-M206         TO  WK-URIKIN
                 ADD  PSJ-M207         TO  WK-TOKURKIN
     END-EVALUATE.
*
 MAIN010.
*    ダイユーエイト 発注情報保存データ読込み
     PERFORM DYPSURL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ＰＯＳ売上情報リストデータ編集
****************************************************************
 DYPSLTL1-WRITE1-SEC   SECTION.
*
     MOVE     SPACE           TO   PSL-REC.
     INITIALIZE                    PSL-REC.
     MOVE     PSJ-F01         TO   PSL-F01.
     MOVE     PSJ-F02         TO   PSL-F02.
     MOVE     PSJ-F05         TO   PSL-F03.
     MOVE     PSJ-F04         TO   PSL-F04.
     MOVE     PSJ-H07         TO   PSL-F06.
     MOVE     PARA-SURID      TO   PSL-F07.
     MOVE     PARA-EURID      TO   PSL-F08.
*****COMPUTE  WK-GENKA  =   PSJ-M210  /  100.
*****MOVE     WK-GENKA        TO   PSL-F09.
     MOVE     PSJ-M210        TO   PSL-F09.
     MOVE     PSJ-M211        TO   PSL-F10.
     MOVE     PSJ-M213        TO   PSL-F11.
     MOVE     PSJ-M212        TO   PSL-F12.
     MOVE     PSJ-M206        TO   PSL-F13.
     MOVE     PSJ-M207        TO   PSL-F14.
*
*商品名カナ取得転送
*  商品変換テーブル検索
     MOVE    SPACE         TO        INV-FLG
     MOVE    PSJ-F01       TO        TBL-F01
     MOVE    PSJ-F04(1:13) TO        TBL-F02
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
       INITIALIZE                  TBL-REC
         MOVE    "INV"     TO        INV-FLG
     END-READ
*商品名　　　
*    商品名称マスタ検索
     IF  INV-FLG  =   SPACE
         MOVE        SPACE     TO        MEI-REC
         INITIALIZE                      MEI-REC
         MOVE        TBL-F031  TO        MEI-F011
         MOVE        TBL-F032  TO        MEI-F012
         READ    MEIMS1
           INVALID
*        商品名
             MOVE    SPACE           TO  PSL-F05
           NOT INVALID
             MOVE    MEI-F03         TO  PSL-F05
         END-READ
     END-IF
*
     WRITE  PSL-REC.
     ADD    1                        TO  O-CNT1.
*
 DYPSLTL1-WRITE1-EXIT.
     EXIT.
****************************************************************
*　　ＰＯＳ売上情報集計リストデータ編集
****************************************************************
 DYPSLTL1-WRITE2-SEC   SECTION.
*
     MOVE     SPACE           TO   PSL-REC.
     INITIALIZE                    PSL-REC.
     MOVE     BRK-TORICD      TO   PSL-F01.
     MOVE     BRK-MISECD      TO   PSL-F02.
     MOVE     ZERO            TO   PSL-F03.
     MOVE     BRK-JANCD       TO   PSL-F04.
     MOVE     PSJ-H07         TO   PSL-F06.
     MOVE     PARA-SURID      TO   PSL-F07.
     MOVE     PARA-EURID      TO   PSL-F08.
     MOVE     ZERO            TO   PSL-F09.
     MOVE     ZERO            TO   PSL-F10.
     MOVE     WK-URISU        TO   PSL-F11.
     MOVE     WK-TOKURSU      TO   PSL-F12.
     MOVE     WK-URIKIN       TO   PSL-F13.
     MOVE     WK-TOKURKIN     TO   PSL-F14.
*
*商品名カナ取得転送
*  商品変換テーブル検索
     MOVE    SPACE         TO        INV-FLG
     MOVE    PSJ-F01       TO        TBL-F01
     MOVE    PSJ-F04(1:13) TO        TBL-F02
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
       INITIALIZE                  TBL-REC
         MOVE    "INV"     TO        INV-FLG
     END-READ
*商品名　　　
*    商品名称マスタ検索
     IF  INV-FLG  =   SPACE
         MOVE        SPACE     TO        MEI-REC
         INITIALIZE                      MEI-REC
         MOVE        TBL-F031  TO        MEI-F011
         MOVE        TBL-F032  TO        MEI-F012
         READ    MEIMS1
           INVALID
*        商品名
             MOVE    SPACE           TO  PSL-F05
           NOT INVALID
             MOVE    MEI-F03         TO  PSL-F05
         END-READ
     END-IF
*
     WRITE  PSL-REC.
     ADD    1                        TO  O-CNT2.
*
 DYPSLTL1-WRITE2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ｳﾘｱｹﾞｼﾞｮｳﾎｳF    READ  CNT = " READ-CNT UPON CONS.
     DISPLAY "ｳﾘｱｹﾞｼﾞｮｳﾎｳ      ﾃﾞｰﾀ CNT = " O-CNT1   UPON CONS.
     DISPLAY "ｳﾘｱｹﾞｼﾞｮｳﾎｳ ｼｭｳｹｲﾃﾞｰﾀ CNT = " O-CNT2   UPON CONS.
*
     CLOSE    DYPSURF   SHOTBL1   MEIMS1  DYPSLTL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
