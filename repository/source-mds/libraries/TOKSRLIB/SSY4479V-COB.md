# SSY4479V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY4479V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ビバホーム　　　　　　　　　　　　*
*    業務名　　　　　　　：　流通ＢＭＳ　　　　　　　　　　　　*
*    モジュール名　　　　：　納品書発行データ作成　　　        *
*    作成日／更新日　　　：　2022/03/15                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を納品明細書形式に抽出する。　　　*
*                            ※Ｍ２レコードのみ出力　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY4479V.
*                  流用:SSY5038B.TOKSLIBS
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/03/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票発行ワーク　　　　　　　　　　　　　　　　　
     SELECT   WKDENVH1  ASSIGN    TO        DA-01-VI-WKDENVH1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F011  DEN-F012
                                            DEN-F013  DEN-F02
                                            DEN-F308  DEN-F346
                                            DEN-F342  DEN-F302
                                            DEN-F402
                        FILE      STATUS    DEN-STATUS.
*納品明細発行データ
     SELECT   VHNOMEI1  ASSIGN    TO        DA-01-VI-VHNOMEI1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       LST-F01   LST-F02
                                            LST-F03   LST-F04
                                            LST-F05   LST-F07
                        FILE      STATUS    LST-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*伝票発行ワーク
******************************************************************
 FD  WKDENVH1
                        LABEL RECORD   IS   STANDARD.
     COPY     WKDENVH1   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*納品明細発行データ
******************************************************************
 FD  VHNOMEI1            LABEL RECORD   IS   STANDARD.
     COPY     VHNOMEI1   OF        XFDLIB
              JOINING   LST       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ステータス領域
 01  WK-ST.
     03  DEN-STATUS          PIC  X(02).
     03  LST-STATUS          PIC  X(02).
*フラグ領域
 01  WK-FLG.
     03  END-FLG             PIC  X(03)     VALUE  SPACE.
     03  KEP-FLG             PIC  X(01)     VALUE  SPACE.
*カウント領域
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  WT-CNT              PIC  9(08)     VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)     VALUE  ZERO.
     03  PAGE-CNT            PIC  9(05)     VALUE  ZERO.
     03  SEQ-CNT             PIC  9(05)     VALUE  ZERO.
     03  TAISYO-CNT          PIC  9(08)     VALUE  ZERO.
     03  WK-LINE-CNT         PIC S9(02)     VALUE  ZERO.
     03  WK-MEIS-CNT         PIC S9(02)     VALUE  ZERO.
     03  IZ                  PIC  9(03)     VALUE  ZERO.
     03  IXA                 PIC  9(03)     VALUE  ZERO.
*INVALIDフラグ
 01  WK-INV-FLG.
     03  KHJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  VHNOMEI1-INV-FLG     PIC  X(03)     VALUE  SPACE.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*ブレイクＫＥＹ領域
 01  WK-BREAK-KEY.
     03  WK-TOKCD            PIC  9(08)     VALUE  ZERO.
     03  WK-TENCD            PIC  9(05)     VALUE  ZERO.
     03  WK-NOUDT            PIC  9(08)     VALUE  ZERO.
     03  WK-BUNRUI           PIC  X(04)     VALUE  SPACE.
     03  WK-SOKCD            PIC  X(02)     VALUE  SPACE.
*    03  WK-DENNO            PIC  9(09)     VALUE  ZERO.
     03  WK-DENNO-8          PIC  9(08)     VALUE  ZERO.
     03  WK-DENNO-R   REDEFINES   WK-DENNO-8.
         05  WK-DENNO-0      PIC  X(02).
         05  WK-DENNO        PIC  X(06).
*店舗ＣＤ桁数変換
 01  WK-TEN-AREA.
     03  WK-DEN308-5         PIC  9(05)     VALUE  ZERO.
     03  WK-DEN308-R  REDEFINES   WK-DEN308-5.
         05  WK-DEN308-0     PIC  X(01).
         05  WK-DEN308       PIC  X(04).
*伝票番号桁数変換
 01  WK-DEN-AREA.
     03  WK-DEN302-8         PIC  9(08)     VALUE  ZERO.
     03  WK-DEN302-R  REDEFINES   WK-DEN302-8.
         05  WK-DEN302-0     PIC  X(02).
         05  WK-DEN302       PIC  X(06).
*行番号桁数変換
 01  WK-GYO-AREA.
     03  WK-DEN402           PIC  X(02)     VALUE  SPACE.
     03  WK-DEN402-R  REDEFINES   WK-DEN402.
         05  WK-DEN402-9     PIC  9(02).
*メッセージ領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY4479V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SSY4479V".
         05  FILLER          PIC   X(11)  VALUE
                                          " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SSY4479V".
         05  FILLER          PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  AB-FILE         PIC   X(08).
         05  FILLER          PIC   X(06)  VALUE " ST = ".
         05  AB-STS          PIC   X(02).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(07)  VALUE " SEC = ".
         05  S-NAME          PIC   X(30).
     03  MSG-IN.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT          PIC   9(06).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT         PIC   9(06).
         05  FILLER          PIC   X(05)  VALUE " *** ".
*欠品有り時　ページ先頭レコード更新ＫＥＹ保存
 01  WK-LST-KEY.
     03  WK-LST-F01          PIC   9(08)   VALUE  ZERO.
     03  WK-LST-F02          PIC   9(04)   VALUE  ZERO.
     03  WK-LST-F03          PIC   9(08)   VALUE  ZERO.
     03  WK-LST-F04          PIC   X(02)   VALUE  SPACE.
     03  WK-LST-F05          PIC   9(05)   VALUE  ZERO.
     03  WK-LST-F07          PIC   9(05)   VALUE  ZERO.
*ＦＬレコード　内部ワーク
 01  WK-FL-REC.
     03  WK-FL-F01           PIC   9(08)   VALUE  ZERO.
     03  WK-FL-F02           PIC   9(05)   VALUE  ZERO.
     03  WK-FL-F03           PIC   9(08)   VALUE  ZERO.
     03  WK-FL-F04           PIC   X(04)   VALUE  SPACE.
     03  WK-FL-F05           PIC   X(01)   VALUE  SPACE.
*Ｍ１レコード　内部ワーク
 01  WK-M1-REC.
     03  WK-M1-F01           PIC   9(08)   VALUE  ZERO.
     03  WK-M1-F02           PIC   9(06)   VALUE  ZERO.
     03  WK-M1-F03           PIC   N(06)   VALUE  SPACE.
     03  WK-M1-F04           PIC   X(01)   VALUE  SPACE.
     03  WK-M1-F05           PIC   X(01)   VALUE  SPACE.
     03  WK-M1-F06           PIC   X(01)   VALUE  SPACE.
*Ｍ１レコード　内部ワーク
 01  WK-M2-REC.
     03  WK-M2-F01           PIC  9(02)    VALUE  ZERO.
     03  WK-M2-F02           PIC  X(25)    VALUE  SPACE.
     03  WK-M2-F03           PIC  X(15)    VALUE  SPACE.
     03  WK-M2-F04           PIC  X(08)    VALUE  SPACE.
     03  WK-M2-F05           PIC  X(13)    VALUE  SPACE.
     03  WK-M2-F06           PIC  9(04)    VALUE  ZERO.
     03  WK-M2-F07           PIC  9(05)    VALUE  ZERO.
     03  WK-M2-F08           PIC  9(05)    VALUE  ZERO.
     03  WK-M2-F09           PIC  9(05)    VALUE  ZERO.
     03  WK-M2-F10           PIC  X(02)    VALUE  SPACE.
     03  WK-M2-F11           PIC  9(02)    VALUE  ZERO.
*    漢字変換
 01  WK-KANJI.
     03  KANJI                    PIC  N(10)  VALUE
         NC"１２３４５６７８９０".
 01  WK-KANJIR                    REDEFINES   WK-KANJI.
     03  WK-SU                    OCCURS      10.
         05  SU                   PIC  N(01).
 01  WK-HENKAN-KANJI.
     03  WK-HENKAN-N.
         05  WK-HENKAN-1          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-2          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-3          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-4          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-5          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-6          PIC  N(01)  VALUE  SPACE.
 01  WK-HENKAN                    PIC  9(06).
*明細行数変換領域
 01  MEIS-GYO                     PIC  X(02).
 01  MEIS-GYO-R                   REDEFINES  MEIS-GYO.
     03  MEIS-GYO-WK              PIC  9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   WKDENVH1.
     MOVE      "WKDENVH1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   VHNOMEI1.
     MOVE      "VHNOMEI1"   TO   AB-FILE.
     MOVE      LST-STATUS   TO   AB-STS.
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
*
     MOVE     "PROCESS-START"     TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
     STOP  RUN.
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     WKDENVH1.
     OPEN     I-O       VHNOMEI1.
*開始メッセージ出力
     DISPLAY  MSG-START UPON CONS.
*ワークエリア初期化
     INITIALIZE   WK-FLG   WK-CNT   WK-BREAK-KEY.
     MOVE     "00"      TO        WK-DENNO-0.
     MOVE     "000000"  TO        WK-DENNO.
     MOVE     SPACE     TO        WK-INV-FLG.
*行数をセット（初期値）
*----MOVE     77        TO        LINE-CNT.
     MOVE     85        TO        LINE-CNT.
*システム日付編集（システム日付により、日付を８桁に変換）
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
*
*    伝票発行ワークＲＥＡＤ
     PERFORM WKDENVH1-READ-SEC.
*
     IF      END-FLG NOT = "END"
             MOVE  DEN-F013   TO   WK-TOKCD
             MOVE  DEN-F308(1:4)   TO   WK-DEN308
             MOVE   "0"            TO   WK-DEN308-0
             MOVE  WK-DEN308-5     TO   WK-TENCD
             MOVE  DEN-F346   TO   WK-NOUDT
             MOVE  DEN-F342   TO   WK-BUNRUI
             MOVE  DEN-F02    TO   WK-SOKCD
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     MOVE     DEN-F308(1:4)       TO   WK-DEN308.
     MOVE     "0"                 TO   WK-DEN308-0.
*
*取引先ＣＤ、店舗ＣＤ、納品日、分類ＣＤがブレイク
*ＦＬレコードセット処理へ（伝票ヘッダ）
     IF       WK-TOKCD  NOT =  DEN-F013
     OR       WK-TENCD  NOT =  WK-DEN308-5
     OR       WK-NOUDT  NOT =  DEN-F346
     OR       WK-BUNRUI NOT =  DEN-F342
     OR       WK-SOKCD  NOT =  DEN-F02
*T↓
*         DISPLAY "DEN-F302 = " DEN-F302 UPON CONS
*         DISPLAY "PAGE-CNT = " PAGE-CNT UPON CONS
*T↑
              PERFORM   FL-SEC
     END-IF.
*Ｍ１レコードセット処理へ（明細ヘッダ）
*    DISPLAY "WK-DENNO = " WK-DENNO  UPON CONS.
*    DISPLAY "DEN-F302 = " DEN-F302(1:6)   UPON CONS.
     IF       WK-DENNO  NOT =  DEN-F302(1:6)
              PERFORM M1-SEC
     END-IF.
*Ｍ２レコード出力処理へ（明細）
     PERFORM  M2-SEC.
*
     PERFORM WKDENVH1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*ＦＬレコードセット処理
****************************************************************
 FL-SEC                SECTION.
*
     MOVE     "FL-SEC"             TO  S-NAME.
*頁がゼロ以上の場合
     IF  PAGE-CNT > ZERO
     AND KEP-FLG = "1"
         PERFORM VHNOMEI1-REWRITE-SEC
     END-IF.
*レコード初期化
     MOVE      SPACE               TO  LST-REC.
     INITIALIZE                        LST-REC.
*頁カウントＵＰ
     ADD       1                   TO  PAGE-CNT.
*ＳＥＱに０をセット
     MOVE      ZERO                TO  SEQ-CNT.
*伝票ヘッダ情報を作成
     MOVE      SPACE               TO  WK-FL-REC  LST-REC.
     INITIALIZE                        WK-FL-REC  LST-REC.
     PERFORM                           REC-INIT-SEC.
*共通フィールド
* 受信日
     MOVE      DEN-F011            TO  LST-F01  WK-LST-F01.
* 受信時間
     MOVE      DEN-F012            TO  LST-F02  WK-LST-F02.
* 受信取引先
     MOVE      DEN-F013            TO  LST-F03  WK-LST-F03.
* 倉庫ＣＤ
     MOVE      DEN-F02             TO  LST-F04  WK-LST-F04.
* 頁
     MOVE      PAGE-CNT            TO  LST-F05  WK-LST-F05.
* 総頁
     MOVE      ZERO                TO  LST-F06.
* SEQ
     MOVE      ZERO                TO  LST-F07  WK-LST-F07.
* レコード区分
     MOVE      "FL"                TO  LST-F08.
* 行数
     MOVE      13                  TO  LST-F09.
*FLフィールド
* 取引先ＣＤ
*----MOVE      DEN-F323            TO  WK-FL-F01.
     MOVE      DEN-F013            TO  LST-FL01.
* 取引先名
     MOVE      NC"株式会社サカタのタネ"  TO  LST-FL02.
* 店舗ＣＤ
*----MOVE      DEN-F308            TO  WK-FL-F02.
     MOVE      DEN-F308(1:4)       TO  WK-DEN308
     MOVE      "0"                 TO  WK-DEN308-0.
     MOVE      WK-DEN308-5         TO  LST-FL03.
* 店舗名
     MOVE      DEN-F310            TO  LST-FL04.
* 納品日
*----MOVE      DEN-F346            TO  WK-FL-F03.
     MOVE      DEN-F346            TO  LST-FL05.
* 分類ＣＤ
*----MOVE      DEN-F342            TO  WK-FL-F04.
     MOVE      DEN-F342            TO  LST-FL06.
* 欠品区分
*    DISPLAY "PAGE-CNT = " PAGE-CNT UPON CONS.
*    DISPLAY "KEP-FLG  = " KEP-FLG  UPON CONS.
*----IF        KEP-FLG  NOT =  SPACE
*----          MOVE   "1"          TO  WK-FL-F05
*----END-IF.
     IF        KEP-FLG  NOT =  SPACE
               MOVE   "1"          TO  LST-FL07
     END-IF.
*
*----MOVE      WK-FL-REC           TO  LST-F10.
*出力する。
*----WRITE     LST-REC.
*----ADD       1                   TO  WT-CNT.
*ＫＥＹを入れ替える
     MOVE      DEN-F013            TO  WK-TOKCD.
     MOVE      DEN-F308(1:4)       TO  WK-DEN308.
     MOVE      "0"                 TO  WK-DEN308-0.
     MOVE      WK-DEN308-5         TO  WK-TENCD.
     MOVE      DEN-F346            TO  WK-NOUDT.
     MOVE      DEN-F342            TO  WK-BUNRUI.
     MOVE      DEN-F02             TO  WK-SOKCD.
*欠品区分を初期化する。
     MOVE      SPACE               TO  KEP-FLG.
*行カウントに１０をセット
*----MOVE      13                  TO  LINE-CNT.
     MOVE      10                  TO  LINE-CNT.
*
 FL-EXIT.
     EXIT.
****************************************************************
*Ｍ１レコードセット処理
****************************************************************
 M1-SEC                SECTION.
*
     MOVE     "M1-SEC"             TO  S-NAME.
*レコード初期化
*----MOVE      SPACE               TO  LST-REC.
*----INITIALIZE                        LST-REC.
*----PERFORM                           REC-INIT-SEC.
*１伝票に印字できない場合のチェック
*　次の伝票印字に必要な行数算出
     MOVE      DEN-F303(1:2)    TO   MEIS-GYO.
*----COMPUTE   WK-MEIS-CNT  =  ( MEIS-GYO-WK * 3 ) + 2.
     COMPUTE   WK-MEIS-CNT  =  ( MEIS-GYO-WK * 2 ) + 2.
*　あと何行印字できるか算出
*----COMPUTE   WK-LINE-CNT  =  79   -  LINE-CNT.
     COMPUTE   WK-LINE-CNT  =  87   -  LINE-CNT.
*　次の伝票１枚分印字出来るか判断
***  DISPLAY "WK-MEIS-CNT = " WK-MEIS-CNT.
***  DISPLAY "WK-LINE-CNT = " WK-LINE-CNT.
     IF       WK-MEIS-CNT  >  WK-LINE-CNT
***  DISPLAY "WK-MEIS-CNT = " WK-MEIS-CNT  UPON CONS
***  DISPLAY "WK-LINE-CNT = " WK-LINE-CNT  UPON CONS
**************DISPLAY "FL2"
*T↓
*         DISPLAY "M1 DEN-F302    = " DEN-F302    UPON CONS
*         DISPLAY "M1 PAGE-CNT    = " PAGE-CNT    UPON CONS
*         DISPLAY "M1 WK-MEIS-CNT = " WK-MEIS-CNT UPON CONS
*         DISPLAY "M1 WK-LINE-CNT = " WK-LINE-CNT UPON CONS
*T↑
               PERFORM   FL-SEC
*----          MOVE      SPACE         TO  LST-REC
*----          INITIALIZE                  LST-REC
*----          PERFORM                     REC-INIT-SEC
     END-IF.
*ＳＥＱ加算
*----ADD       1                   TO  SEQ-CNT.
*共通フィールド
* 受信日付
     MOVE      DEN-F011            TO  LST-F01.
* 受信時間
     MOVE      DEN-F012            TO  LST-F02.
* 受信取引先
     MOVE      DEN-F013            TO  LST-F03.
* 倉庫ＣＤ
     MOVE      WK-SOKCD            TO  LST-F04.
* 頁
     MOVE      PAGE-CNT            TO  LST-F05.
* 総頁
     MOVE      ZERO                TO  LST-F06.
* SEQ
     MOVE      SEQ-CNT             TO  LST-F07.
* レコード区分
     MOVE      "M1"                TO  LST-F08.
* 行数
     MOVE      2                   TO  LST-F09.
*M1フィールド
* 発注日
*----MOVE      DEN-F15             TO  WK-M1-F01.
     MOVE      DEN-F344            TO  LST-M101.
* 伝票番号
*----MOVE      DEN-F302            TO  WK-M1-F02.
     MOVE      DEN-F302(1:6)       TO  WK-DEN302.
     MOVE      "00"                TO  WK-DEN302-0.
     MOVE      WK-DEN302-8         TO  LST-M102.
* 伝票番号日本語
     MOVE      SPACE               TO  WK-HENKAN-N.
     MOVE      DEN-F302(1:6)       TO  WK-HENKAN.
     PERFORM  VARYING   IZ   FROM  1 BY 1  UNTIL  IZ > 6
              PERFORM   KANJI-HENKAN-SEC
     END-PERFORM.
*----MOVE     WK-HENKAN-N          TO  WK-M1-F03.
     MOVE     WK-HENKAN-N          TO  LST-M103.
* 客注
     IF        DEN-F352  =  "04"
*----          MOVE    "1"         TO  WK-M1-F04
               MOVE    "1"         TO  LST-M104
     ELSE
*----          MOVE    SPACE       TO  WK-M1-F04
               MOVE    SPACE       TO  LST-M104
     END-IF.
* 緊急
*----PERFORM   KHJOHOF-READ-SEC.
*----IF        KHJOHOF-INV-FLG = SPACE
*----          IF  KHJ-HD19  =  "1"
*----              MOVE "1"        TO WK-M1-F05
*----          ELSE
*----              MOVE SPACE      TO WK-M1-F05
*----          END-IF
*----ELSE
*----          MOVE     SPACE      TO WK-M1-F05
*----END-IF.
* 特売
*----IF        DEN-F351  =  "1"
     IF        DEN-F351  =  "03"
*----          MOVE    "1"         TO WK-M1-F06
               MOVE    "1"         TO LST-M106
     ELSE
*----          MOVE    SPACE       TO WK-M1-F06
               MOVE    SPACE       TO LST-M106
     END-IF.
*
*----MOVE      WK-M1-REC           TO  LST-F10.
*出力する。
*----WRITE     LST-REC.
*----ADD       1                   TO  WT-CNT.
*ＫＥＹを入れ替える
     MOVE      DEN-F302(1:6)       TO  WK-DENNO.
     MOVE      "00"                TO  WK-DENNO-0.
     MOVE      DEN-F02             TO  WK-SOKCD.
*行カウントに１をセット
*----ADD       2                   TO  LINE-CNT.
     ADD       1                   TO  LINE-CNT.
*
 M1-EXIT.
     EXIT.
****************************************************************
*Ｍ２レコード出力処理
****************************************************************
 M2-SEC                SECTION.
*
     MOVE     "M2-SEC"             TO  S-NAME.
*レコード初期化
*----MOVE      SPACE               TO  LST-REC.
*----INITIALIZE                        LST-REC.
*----PERFORM                           REC-INIT-SEC.
*ＳＥＱ加算
     ADD       1                   TO  SEQ-CNT.
*共通フィールド
* 受信日付
     MOVE      DEN-F011            TO  LST-F01.
* 受信時間
     MOVE      DEN-F012            TO  LST-F02.
* 受信取引先
     MOVE      DEN-F013            TO  LST-F03.
* 倉庫ＣＤ
     MOVE      WK-SOKCD            TO  LST-F04.
* 頁
     MOVE      PAGE-CNT            TO  LST-F05.
* 総頁
     MOVE      ZERO                TO  LST-F06.
* SEQ
     MOVE      SEQ-CNT             TO  LST-F07.
* レコード区分
     MOVE      "M2"                TO  LST-F08.
* 行数
     MOVE      3                   TO  LST-F09.
*M2フィールド
* 行数
*--- MOVE      DEN-ME03            TO  WK-M2-F01.
     MOVE      DEN-F402(1:2)       TO  WK-DEN402.
     MOVE      WK-DEN402-9         TO  LST-M201.
* 商品名１、２
*----PERFORM   KHJOHOF-READ-SEC.
*----IF        KHJOHOF-INV-FLG = SPACE
*----          MOVE  KHJ-ME15      TO  WK-M2-F02
*----          MOVE  KHJ-ME16      TO  WK-M2-F03
*----ELSE
*----          MOVE  ALL "*"       TO  WK-M2-F02
*----          MOVE  ALL "*"       TO  WK-M2-F03
*----END-IF.
     MOVE      DEN-F416(1:15)      TO  LST-M202.
     MOVE      DEN-F416(16:10)     TO  LST-M203.
     MOVE      DEN-F418(1:5)       TO  LST-M203(11:5).
* 商品ＣＤ
*----MOVE      DEN-ME04            TO  WK-M2-F04.
     MOVE      DEN-F413(1:8)       TO  LST-M204.
* ＪＡＮＣＤ
*----MOVE      DEN-ME18            TO  WK-M2-F05.
     MOVE      DEN-F412(2:13)      TO  LST-M205.
* 発注単位
*----MOVE      DEN-ME06            TO  WK-M2-F06.
     MOVE      DEN-F450            TO  LST-M206.
* 発注数
*----MOVE      DEN-ME09            TO  WK-M2-F07.
     MOVE      DEN-F453            TO  LST-M207.
* 出荷数
*----MOVE      DEN-ME07            TO  WK-M2-F08.
     MOVE      DEN-F604            TO  LST-M208.
* 欠品数
*----MOVE      DEN-ME19            TO  WK-M2-F09.
     MOVE      DEN-F606            TO  LST-M209.
* 欠品区分
* 欠品判定
*----IF        DEN-ME20  NOT =  "00"
***************DISPLAY "PAGE-CNT = " PAGE-CNT UPON CONS
***************DISPLAY "DEN-ME04 = " DEN-ME04 UPON CONS
*----          MOVE  DEN-ME20      TO  WK-M2-F10
*----          MOVE  "1"           TO  KEP-FLG
*----ELSE
*----          MOVE  SPACE         TO  WK-M2-F10
*----END-IF.
     IF        DEN-F608  NOT =  "00"
***************DISPLAY "PAGE-CNT = " PAGE-CNT UPON CONS
***************DISPLAY "DEN-F608 = " DEN-ME04 UPON CONS
               MOVE  DEN-F608      TO  LST-M210
               MOVE  "1"           TO  KEP-FLG
     ELSE
*#2022/10/03 NAV ST数量が異なる場合は欠品扱い
***************IF  LST-M207  NOT =  LST-M208
***************IF  DEN-F453  NOT =  DEN-F604
     DISPLAY "DEN-F606 = " DEN-F060 UPON CONS
               IF  DEN-F606  <>     ZERO
            DISPLAY "AAA" UPON CONS
                   MOVE "21"       TO  LST-M210
                   MOVE  "1"       TO  KEP-FLG
               ELSE
                   MOVE  SPACE         TO  LST-M210
               END-IF
************** MOVE  SPACE         TO  LST-M210
*#2022/10/03 NAV ED数量が異なる場合は欠品扱い
     END-IF.
* 伝票行数
*----MOVE      MEIS-GYO-WK         TO  WK-M2-F11.
     MOVE      MEIS-GYO-WK         TO  LST-M211.
*
*----MOVE      WK-M2-REC           TO  LST-F10.
*出力する。
     WRITE     LST-REC.
     ADD       1                   TO  WT-CNT.
*行カウントに2をセット
*----ADD       3                   TO  LINE-CNT.
     ADD       2                   TO  LINE-CNT.
*
 M2-EXIT.
     EXIT.
****************************************************************
*ＴＰレコードセット処理
****************************************************************
 TP-SEC                SECTION.
*
     MOVE     "TP-SEC"             TO  S-NAME.
*レコード初期化
     MOVE      SPACE               TO  LST-REC.
     INITIALIZE                        LST-REC.
     PERFORM                           REC-INIT-SEC.
*共通フィールド
* 受信日付
     MOVE      DEN-F011            TO  LST-F01.
* 受信時間
     MOVE      DEN-F012            TO  LST-F02.
* 受信取引先
     MOVE      DEN-F013            TO  LST-F03.
* 倉庫ＣＤ
     MOVE      SPACE               TO  LST-F04.
* 頁
     MOVE      ZERO                TO  LST-F05.
* 総頁
     MOVE      PAGE-CNT            TO  LST-F06.
* SEQ
     MOVE      ZERO                TO  LST-F07.
* レコード区分
     MOVE      "TP"                TO  LST-F08.
* 行数
     MOVE      ZERO                TO  LST-F09.
*出力する。
*----WRITE     LST-REC.
*----ADD       1                   TO  WT-CNT.
*
 TP-EXIT.
     EXIT.
****************************************************************
*　　セット：カンマ・制御バイト　　　　　　　　　　　　　　　*
****************************************************************
 REC-INIT-SEC  SECTION.
*
     MOVE     X"28"                TO  LST-FL02S
                                       LST-FL04S
                                       LST-M103S
                                       LST-M202S
                                       LST-M203S.
     MOVE     X"29"                TO  LST-FL02E
                                       LST-FL04E
                                       LST-M103E
                                       LST-M202E
                                       LST-M203E.
     MOVE      ","                 TO  LST-K01
                                       LST-K02
                                       LST-K03
                                       LST-K04
                                       LST-K05
                                       LST-K06
                                       LST-K07
                                       LST-K08
                                       LST-K09
                                       LST-FIL1K
                                       LST-FL01K
                                       LST-FL02K
                                       LST-FL03K
                                       LST-FL04K
                                       LST-FL05K
                                       LST-FL06K
                                       LST-FL07K
                                       LST-FIL2K
                                       LST-M101K
                                       LST-M102K
                                       LST-M103K
                                       LST-M104K
                                       LST-M105K
                                       LST-M106K
                                       LST-FIL3K
                                       LST-M201K
                                       LST-M202K
                                       LST-M203K
                                       LST-M204K
                                       LST-M205K
                                       LST-M206K
                                       LST-M207K
                                       LST-M208K
                                       LST-M209K
                                       LST-M210K
                                       LST-M211K
                                       LST-FIL4K.
 REC-INIT-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*頁がゼロ以上の場合
     IF  PAGE-CNT > ZERO
     AND KEP-FLG = "1"
         PERFORM VHNOMEI1-REWRITE-SEC
     END-IF.
*総頁行出力
     IF   WT-CNT >  ZERO
          PERFORM TP-SEC
     END-IF.
*
     DISPLAY "WKDENVH1         READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "VHNOMEI1       OUTPUT CNT = " WT-CNT    UPON CONS.
*
     CLOSE     WKDENVH1  VHNOMEI1.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　伝票発行ワーク読込
****************************************************************
 WKDENVH1-READ-SEC    SECTION.
*
     READ     WKDENVH1
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  WKDENVH1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*WKDENVH1010.
*----バッチ番号のチェック
*----IF       DEN-F011  =  DEN-F01
*----AND      DEN-F012  =  DEN-F02
*----AND      DEN-F013  =  DEN-F013
*----         CONTINUE
*----ELSE
*----         MOVE     "END"        TO  END-FLG
*----         GO                    TO  WKDENVH1-READ-EXIT
*----END-IF.
*WKDENVH1020.
*----倉庫ＣＤチェック
*----IF       PARA-SOKO  =  SPACE
*----         CONTINUE
*----ELSE
*----         IF   PARA-SOKO   =  DEN-F02
*----              CONTINUE
*----         ELSE
*----              MOVE     "END"   TO  END-FLG
*----              GO               TO  WKDENVH1-READ-EXIT
*----         END-IF
*----END-IF.
*WKDENVH1030.
*----店舗ＣＤ範囲チェック
*----IF       PARA-TENST  <=   DEN-F308
*----AND      PARA-TENED  >=   DEN-F308
*----         CONTINUE
*----ELSE
*----         ADD     1             TO   TAISYO-CNT
*----         GO                    TO   WKDENVH1-READ-SEC
*----END-IF.
*WKDENVH1040.
*----納品日範囲チェック
*----IF       PARA-NOUST  <=   DEN-F346
*----AND      PARA-NOUED  >=   DEN-F346
*----         CONTINUE
*----ELSE
*----         ADD     1             TO   TAISYO-CNT
*----         GO                    TO   WKDENVH1-READ-SEC
*----END-IF.
*WKDENVH1050.
*----伝票番号範囲チェック
*----IF       PARA-DENST  <=   DEN-F302
*----AND      PARA-DENED  >=   DEN-F302
*----         CONTINUE
*----ELSE
*----         ADD     1             TO   TAISYO-CNT
*----         GO                    TO   WKDENVH1-READ-SEC
*----END-IF.
*
 WKDENVH1-READ-EXIT.
     EXIT.
****************************************************************
*　　欠品区分更新処理
****************************************************************
 VHNOMEI1-REWRITE-SEC SECTION.
*
     MOVE     SPACE          TO   LST-REC.
     INITIALIZE                   LST-REC.
     MOVE     WK-LST-F01     TO   LST-F01.
     MOVE     WK-LST-F02     TO   LST-F02.
     MOVE     WK-LST-F03     TO   LST-F03.
     MOVE     WK-LST-F04     TO   LST-F04.
     MOVE     WK-LST-F05     TO   LST-F05.
*----MOVE     WK-LST-F07     TO   LST-F07.
     MOVE     1              TO   LST-F07.
     READ     VHNOMEI1    INVALID
              MOVE    "INV"       TO   VHNOMEI1-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   VHNOMEI1-INV-FLG
     END-READ.
*
     IF  VHNOMEI1-INV-FLG = "INV"
*--------DISPLAY NC"＃　ＦＬレコード更新異常！！" UPON CONS
         DISPLAY NC"＃　欠品区分　　更新異常！！" UPON CONS
         MOVE    4000             TO   PROGRAM-STATUS
     ELSE
*----    MOVE    "1"              TO   LST-F10(26:1)
         MOVE    "1"              TO   LST-FL07
         REWRITE  LST-REC
     END-IF.
*
     MOVE         SPACE           TO   KEP-FLG.
*
 VHNOMEI1-REWRITE-EXIT.
     EXIT.
*============================================================*
*　　漢字変換処理
*============================================================*
 KANJI-HENKAN-SEC      SECTION.
*    数字を漢字に変換（ワーク変換テーブルにより）
     MOVE    WK-HENKAN(IZ:1)           TO     IXA.
     IF      IXA       =     ZERO
             MOVE      10              TO     IXA
     END-IF.
*
     EVALUATE     IZ
         WHEN     1
                  MOVE       SU(IXA)   TO   WK-HENKAN-1
         WHEN     2
                  MOVE       SU(IXA)   TO   WK-HENKAN-2
         WHEN     3
                  MOVE       SU(IXA)   TO   WK-HENKAN-3
         WHEN     4
                  MOVE       SU(IXA)   TO   WK-HENKAN-4
         WHEN     5
                  MOVE       SU(IXA)   TO   WK-HENKAN-5
         WHEN     6
                  MOVE       SU(IXA)   TO   WK-HENKAN-6
     END-EVALUATE.
*
 KANJI-HENKAN-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
