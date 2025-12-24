# SSY5038B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5038B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　トステムビバ　　　　　　　　　　　*
*    業務名　　　　　　　：　新ＥＤＩオンラインシステム　　　　*
*    モジュール名　　　　：　納品明細発行制御データ作成        *
*    作成日／更新日　　　：　2010/03/30                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を納品明細書形式に抽出する。　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5038B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/03/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*（共通）出荷情報ファイル（ＬＦ４）
     SELECT   KHSYUKF   ASSIGN    TO        DA-01-VI-KHSYUKL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KHS-F01   KHS-F02
                                            KHS-F03   KHS-F04
                                            KHS-F05   KHS-F08
                                            KHS-F13   KHS-F06
                                            KHS-F07
                        FILE      STATUS    KHS-STATUS.
*（共通）基本情報ファイル（ＬＦ４）
     SELECT   KHJOHOF   ASSIGN    TO        DA-01-VI-KHJOHOL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KHJ-F01   KHJ-F02
                                            KHJ-F03   KHJ-F04
                                            KHJ-F05   KHJ-F08
                                            KHJ-F13   KHJ-F06
                                            KHJ-F07
                        FILE      STATUS    KHJ-STATUS.
*（リスト）納品明細発行制御ファイル（ＬＦ１）
     SELECT   LSTNMEF   ASSIGN    TO        DA-01-VI-LSTNMEL1
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
*（共通）出荷情報ファイル（ＬＦ４）
******************************************************************
 FD  KHSYUKF
                        LABEL RECORD   IS   STANDARD.
     COPY     KHSYUKF   OF        XFDLIB
              JOINING   KHS  AS   PREFIX.
*
******************************************************************
*（共通）基本情報ファイル（ＬＦ４）
******************************************************************
 FD  KHJOHOF
                        LABEL RECORD   IS   STANDARD.
     COPY     KHJOHOF   OF        XFDLIB
              JOINING   KHJ  AS   PREFIX.
*
******************************************************************
*（リスト）納品明細発行制御ファイル（ＬＦ１）
******************************************************************
 FD  LSTNMEF            LABEL RECORD   IS   STANDARD.
     COPY     LSTNMEF   OF        XFDLIB
              JOINING   LST       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ステータス領域
 01  WK-ST.
     03  KHS-STATUS          PIC  X(02).
     03  LST-STATUS          PIC  X(02).
     03  KHJ-STATUS          PIC  X(02).
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
     03  LSTNMEF-INV-FLG     PIC  X(03)     VALUE  SPACE.
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
     03  WK-DENNO            PIC  9(09)     VALUE  ZERO.
*メッセージ領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY5038B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SSY5038B".
         05  FILLER          PIC   X(11)  VALUE
                                          " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SSY5038B".
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
*ＦＬレコード更新ＫＥＹ保存
 01  WK-LST-KEY.
     03  WK-LST-F01          PIC   9(08)   VALUE  ZERO.
     03  WK-LST-F02          PIC   9(04)   VALUE  ZERO.
     03  WK-LST-F03          PIC   9(08)   VALUE  ZERO.
     03  WK-LST-F04          PIC   X(02)   VALUE  SPACE.
     03  WK-LST-F05          PIC   9(05)   VALUE  ZERO.
     03  WK-LST-F07          PIC   9(05)   VALUE  ZERO.
*ＦＬレコード
 01  WK-FL-REC.
     03  WK-FL-F01           PIC   9(08)   VALUE  ZERO.
     03  WK-FL-F02           PIC   9(05)   VALUE  ZERO.
     03  WK-FL-F03           PIC   9(08)   VALUE  ZERO.
     03  WK-FL-F04           PIC   X(04)   VALUE  SPACE.
     03  WK-FL-F05           PIC   X(01)   VALUE  SPACE.
*Ｍ１レコード
 01  WK-M1-REC.
     03  WK-M1-F01           PIC   9(08)   VALUE  ZERO.
     03  WK-M1-F02           PIC   9(06)   VALUE  ZERO.
     03  WK-M1-F03           PIC   N(06)   VALUE  SPACE.
     03  WK-M1-F04           PIC   X(01)   VALUE  SPACE.
     03  WK-M1-F05           PIC   X(01)   VALUE  SPACE.
     03  WK-M1-F06           PIC   X(01)   VALUE  SPACE.
*Ｍ１レコード
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
 01  MEIS-GYO                     PIC  X(01).
 01  MEIS-GYO-R                   REDEFINES  MEIS-GYO.
     03  MEIS-GYO-WK              PIC  9(01).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-TENST             PIC   9(05).
 01  PARA-TENED             PIC   9(05).
 01  PARA-NOUST             PIC   9(08).
 01  PARA-NOUED             PIC   9(08).
 01  PARA-DENST             PIC   9(09).
 01  PARA-DENED             PIC   9(09).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-TENST
                                       PARA-TENED
                                       PARA-NOUST
                                       PARA-NOUED
                                       PARA-DENST
                                       PARA-DENED.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KHSYUKF.
     MOVE      "KHSYUKL4"   TO   AB-FILE.
     MOVE      KHS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LSTNMEF.
     MOVE      "LSTNMEL1"   TO   AB-FILE.
     MOVE      LST-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KHJOHOF.
     MOVE      "KHJOHOL4"   TO   AB-FILE.
     MOVE      KHJ-STATUS   TO   AB-STS.
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
     OPEN     INPUT     KHSYUKF  KHJOHOF.
     OPEN     I-O       LSTNMEF.
*開始メッセージ出力
     DISPLAY  MSG-START UPON CONS.
*ワークエリア初期化
     INITIALIZE   WK-FLG   WK-CNT   WK-BREAK-KEY.
     MOVE     SPACE     TO        WK-INV-FLG.
*行数をセット（初期値）
     MOVE     77        TO        LINE-CNT.
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
*    （共通）出荷情報ファイルＳＴＡＲＴ
     MOVE     SPACE          TO   KHS-REC.
     INITIALIZE                   KHS-REC.
     MOVE     PARA-JDATE     TO   KHS-F01.
     MOVE     PARA-JTIME     TO   KHS-F02.
     MOVE     PARA-TORICD    TO   KHS-F03.
     MOVE     PARA-SOKO      TO   KHS-F04.
     MOVE     PARA-TENST     TO   KHS-F05.
     MOVE     PARA-NOUST     TO   KHS-F09.
     START    KHSYUKF   KEY  >=   KHS-F01   KHS-F02   KHS-F03
                                  KHS-F04   KHS-F05   KHS-F08
                                  KHS-F13   KHS-F06   KHS-F07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
              STOP  RUN
     END-START.
*    （共通）出荷情報ファイルＳＴＡＲＴ
     PERFORM KHSYUKF-READ-SEC.
*
     IF      END-FLG NOT = "END"
*************DISPLAY "AAA"
             MOVE  KHS-F03   TO   WK-TOKCD
             MOVE  KHS-F05   TO   WK-TENCD
             MOVE  KHS-F08   TO   WK-NOUDT
             MOVE  KHS-F13   TO   WK-BUNRUI
             MOVE  KHS-F04   TO   WK-SOKCD
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
*取引先ＣＤ、店舗ＣＤ、納品日、分類ＣＤがブレイク
*ＦＬレコード出力処理へ（伝票ヘッダ）
     IF       WK-TOKCD  NOT =  KHS-F03
     OR       WK-TENCD  NOT =  KHS-F05
     OR       WK-NOUDT  NOT =  KHS-F08
     OR       WK-BUNRUI NOT =  KHS-F13
     OR       WK-SOKCD  NOT =  KHS-F04
              PERFORM FL-SEC
     END-IF.
*Ｍ１レコード出力処理へ（明細ヘッダ）
*    DISPLAY "WK-DENNO = " WK-DENNO  UPON CONS.
*    DISPLAY "KHS-F06  = " KHS-F06   UPON CONS.
     IF       WK-DENNO  NOT =  KHS-F06
              PERFORM M1-SEC
     END-IF.
*Ｍ２レコード出力処理へ（明細）
     PERFORM  M2-SEC.
*
     PERFORM KHSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*ＦＬレコード出力処理
****************************************************************
 FL-SEC                SECTION.
*
     MOVE     "FL-SEC"             TO  S-NAME.
*頁がゼロ以上の場合
     IF  PAGE-CNT > ZERO
     AND KEP-FLG = "1"
         PERFORM LSTNMEF-REWRITE-SEC
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
*受信日
     MOVE      PARA-JDATE          TO  LST-F01  WK-LST-F01.
*受信時間
     MOVE      PARA-JTIME          TO  LST-F02  WK-LST-F02.
*受信取引先
     MOVE      PARA-TORICD         TO  LST-F03  WK-LST-F03.
*倉庫ＣＤ
     MOVE      KHS-F04             TO  LST-F04  WK-LST-F04.
*頁
     MOVE      PAGE-CNT            TO  LST-F05  WK-LST-F05.
*総頁
     MOVE      ZERO                TO  LST-F06.
*SEQ
     MOVE      ZERO                TO  LST-F07  WK-LST-F07.
*レコード区分
     MOVE      "FL"                TO  LST-F08.
*行数
     MOVE      13                  TO  LST-F09.
*取引先ＣＤ
     MOVE      KHS-F03             TO  WK-FL-F01.
*店舗ＣＤ
     MOVE      KHS-F05             TO  WK-FL-F02.
*納品日
     MOVE      KHS-F08             TO  WK-FL-F03.
*分類ＣＤ
     MOVE      KHS-F13             TO  WK-FL-F04.
*欠品区分
*    DISPLAY "PAGE-CNT = " PAGE-CNT UPON CONS.
*    DISPLAY "KEP-FLG  = " KEP-FLG  UPON CONS.
     IF        KEP-FLG  NOT =  SPACE
               MOVE   "1"          TO  WK-FL-F05
     END-IF.
*
     MOVE      WK-FL-REC           TO  LST-F10.
*出力する。
     WRITE     LST-REC.
     ADD       1                   TO  WT-CNT.
*ＫＥＹを入れ替える
     MOVE      KHS-F03             TO  WK-TOKCD.
     MOVE      KHS-F05             TO  WK-TENCD.
     MOVE      KHS-F08             TO  WK-NOUDT.
     MOVE      KHS-F13             TO  WK-BUNRUI.
     MOVE      KHS-F04             TO  WK-SOKCD.
*欠品区分を初期化する。
     MOVE      SPACE               TO  KEP-FLG.
*行カウントに１０をセット
     MOVE      13                  TO  LINE-CNT.
*
 FL-EXIT.
     EXIT.
****************************************************************
*Ｍ１レコード出力処理
****************************************************************
 M1-SEC                SECTION.
*
     MOVE     "M1-SEC"             TO  S-NAME.
*レコード初期化
     MOVE      SPACE               TO  LST-REC.
     INITIALIZE                        LST-REC.
*１伝票に印字できない場合のチェック
*後何行印字出来るか判断
     MOVE      KHS-F17      TO   MEIS-GYO.
     COMPUTE   WK-MEIS-CNT  =  ( MEIS-GYO-WK * 3 ) + 2.
*あと幾つ印字出来るか判断
     COMPUTE   WK-LINE-CNT  =  79   -  LINE-CNT.
*伝票１枚分印字出来るか判断
***  DISPLAY "WK-MEIS-CNT = " WK-MEIS-CNT.
***  DISPLAY "WK-LINE-CNT = " WK-LINE-CNT.
     IF        WK-MEIS-CNT  >  WK-LINE-CNT
***  DISPLAY "WK-MEIS-CNT = " WK-MEIS-CNT  UPON CONS
***  DISPLAY "WK-LINE-CNT = " WK-LINE-CNT  UPON CONS
**************DISPLAY "FL2"
               PERFORM  FL-SEC
     END-IF.
*ＳＥＱ加算
     ADD       1                   TO  SEQ-CNT.
*受信日付
     MOVE      PARA-JDATE          TO  LST-F01.
*受信時間
     MOVE      PARA-JTIME          TO  LST-F02.
*受信取引先
     MOVE      PARA-TORICD          TO  LST-F03.
*倉庫ＣＤ
     MOVE      WK-SOKCD            TO  LST-F04.
*頁
     MOVE      PAGE-CNT            TO  LST-F05.
*総頁
     MOVE      ZERO                TO  LST-F06.
*SEQ
     MOVE      SEQ-CNT             TO  LST-F07.
*レコード区分
     MOVE      "M1"                TO  LST-F08.
*行数
     MOVE      2                   TO  LST-F09.
*発注日
     MOVE      KHS-F15             TO  WK-M1-F01.
*伝票番号
     MOVE      KHS-F06             TO  WK-M1-F02.
*伝票番号日本語
     MOVE      SPACE               TO  WK-HENKAN-N.
     MOVE      KHS-F06             TO  WK-HENKAN.
     PERFORM  VARYING   IZ   FROM  1 BY 1  UNTIL  IZ > 6
              PERFORM   KANJI-HENKAN-SEC
     END-PERFORM.
     MOVE     WK-HENKAN-N          TO  WK-M1-F03.
*客注
     IF        KHS-HD17  =  "04"
               MOVE    "1"         TO  WK-M1-F04
     ELSE
               MOVE    SPACE       TO  WK-M1-F04
     END-IF.
*緊急
     PERFORM   KHJOHOF-READ-SEC.
     IF        KHJOHOF-INV-FLG = SPACE
               IF  KHJ-HD19  =  "1"
                   MOVE "1"        TO WK-M1-F05
               ELSE
                   MOVE SPACE      TO WK-M1-F05
               END-IF
     ELSE
               MOVE     SPACE      TO WK-M1-F05
     END-IF.
*特売
     IF        KHS-HM03  =  "1"
               MOVE    "1"         TO WK-M1-F06
     ELSE
               MOVE    SPACE       TO WK-M1-F06
     END-IF.
*
     MOVE      WK-M1-REC           TO  LST-F10.
*出力する。
     WRITE     LST-REC.
     ADD       1                   TO  WT-CNT.
*ＫＥＹを入れ替える
     MOVE      KHS-F06             TO  WK-DENNO.
     MOVE      KHS-F04             TO  WK-SOKCD.
*行カウントに１０をセット
     ADD       2                   TO  LINE-CNT.
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
     MOVE      SPACE               TO  LST-REC.
     INITIALIZE                        LST-REC.
*ＳＥＱ加算
     ADD       1                   TO  SEQ-CNT.
*受信日付
     MOVE      PARA-JDATE          TO  LST-F01.
*受信時間
     MOVE      PARA-JTIME          TO  LST-F02.
*受信取引先
     MOVE      PARA-TORICD          TO  LST-F03.
*倉庫ＣＤ
     MOVE      WK-SOKCD            TO  LST-F04.
*頁
     MOVE      PAGE-CNT            TO  LST-F05.
*総頁
     MOVE      ZERO                TO  LST-F06.
*SEQ
     MOVE      SEQ-CNT             TO  LST-F07.
*レコード区分
     MOVE      "M2"                TO  LST-F08.
*行数
     MOVE      3                   TO  LST-F09.
*行数
     MOVE      KHS-ME03            TO  WK-M2-F01.
*商品名１、２
     PERFORM   KHJOHOF-READ-SEC.
     IF        KHJOHOF-INV-FLG = SPACE
               MOVE  KHJ-ME15      TO  WK-M2-F02
               MOVE  KHJ-ME16      TO  WK-M2-F03
     ELSE
               MOVE  ALL "*"       TO  WK-M2-F02
               MOVE  ALL "*"       TO  WK-M2-F03
     END-IF.
*商品ＣＤ
     MOVE      KHS-ME04            TO  WK-M2-F04.
*ＪＡＮＣＤ
     MOVE      KHS-ME18            TO  WK-M2-F05.
*発注単位
     MOVE      KHS-ME06            TO  WK-M2-F06.
*発注数
     MOVE      KHS-ME09            TO  WK-M2-F07.
*出荷数
     MOVE      KHS-ME07            TO  WK-M2-F08.
*欠品数
     MOVE      KHS-ME19            TO  WK-M2-F09.
*欠品区分
*欠品判定
     IF        KHS-ME20  NOT =  "00"
***************DISPLAY "PAGE-CNT = " PAGE-CNT UPON CONS
***************DISPLAY "KHS-ME04 = " KHS-ME04 UPON CONS
               MOVE  KHS-ME20      TO  WK-M2-F10
               MOVE  "1"           TO  KEP-FLG
     ELSE
               MOVE  SPACE         TO  WK-M2-F10
     END-IF.
*伝票行数
     MOVE      MEIS-GYO-WK         TO  WK-M2-F11.
*
     MOVE      WK-M2-REC           TO  LST-F10.
*出力する。
     WRITE     LST-REC.
     ADD       1                   TO  WT-CNT.
*行カウントに１０をセット
     ADD       3                   TO  LINE-CNT.
*
 M2-EXIT.
     EXIT.
****************************************************************
*ＴＰレコード出力処理
****************************************************************
 TP-SEC                SECTION.
*
     MOVE     "TP-SEC"             TO  S-NAME.
*レコード初期化
     MOVE      SPACE               TO  LST-REC.
     INITIALIZE                        LST-REC.
*受信日付
     MOVE      PARA-JDATE          TO  LST-F01.
*受信時間
     MOVE      PARA-JTIME          TO  LST-F02.
*受信取引先
     MOVE      PARA-TORICD         TO  LST-F03.
*倉庫ＣＤ
     MOVE      SPACE               TO  LST-F04.
*頁
     MOVE      ZERO                TO  LST-F05.
*総頁
     MOVE      PAGE-CNT            TO  LST-F06.
*SEQ
     MOVE      ZERO                TO  LST-F07.
*レコード区分
     MOVE      "TP"                TO  LST-F08.
*行数
     MOVE      ZERO                TO  LST-F09.
*出力する。
     WRITE     LST-REC.
     ADD       1                   TO  WT-CNT.
*
 TP-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*頁がゼロ以上の場合
     IF  PAGE-CNT > ZERO
     AND KEP-FLG = "1"
         PERFORM LSTNMEF-REWRITE-SEC
     END-IF.
*総頁行出力
     IF   WT-CNT >  ZERO
          PERFORM TP-SEC
     END-IF.
*
     DISPLAY "KHSYUKF         READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "LSTNMEF       OUTPUT CNT = " WT-CNT    UPON CONS.
*
     CLOSE     KHSYUKF  KHJOHOF  LSTNMEF.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　（共通）出荷情報ファイル読込
****************************************************************
 KHSYUKF-READ-SEC    SECTION.
*
     READ     KHSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KHSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
 KHSYUKF010.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  KHS-F01
     AND      PARA-JTIME  =  KHS-F02
     AND      PARA-TORICD =  KHS-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  KHSYUKF-READ-EXIT
     END-IF.
 KHSYUKF020.
*    倉庫ＣＤチェック
     IF       PARA-SOKO  =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO   =  KHS-F04
                   CONTINUE
              ELSE
                   MOVE     "END"   TO  END-FLG
                   GO               TO  KHSYUKF-READ-EXIT
              END-IF
     END-IF.
 KHSYUKF030.
*    店舗ＣＤ範囲チェック
     IF       PARA-TENST  <=   KHS-F05
     AND      PARA-TENED  >=   KHS-F05
              CONTINUE
     ELSE
              ADD     1             TO   TAISYO-CNT
              GO                    TO   KHSYUKF-READ-SEC
     END-IF.
 KHSYUKF040.
*    納品日範囲チェック
     IF       PARA-NOUST  <=   KHS-F08
     AND      PARA-NOUED  >=   KHS-F08
              CONTINUE
     ELSE
              ADD     1             TO   TAISYO-CNT
              GO                    TO   KHSYUKF-READ-SEC
     END-IF.
 KHSYUKF050.
*    伝票番号範囲チェック
     IF       PARA-DENST  <=   KHS-F06
     AND      PARA-DENED  >=   KHS-F06
              CONTINUE
     ELSE
              ADD     1             TO   TAISYO-CNT
              GO                    TO   KHSYUKF-READ-SEC
     END-IF.
*
 KHSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KHJOHOF-READ-SEC    SECTION.
*
     MOVE     SPACE          TO   KHJ-REC.
     INITIALIZE                   KHJ-REC.
     MOVE     KHS-F01        TO   KHJ-F01.
     MOVE     KHS-F02        TO   KHJ-F02.
     MOVE     KHS-F03        TO   KHJ-F03.
     MOVE     KHS-F04        TO   KHJ-F04.
     MOVE     KHS-F05        TO   KHJ-F05.
     MOVE     KHS-F08        TO   KHJ-F08.
     MOVE     KHS-F13        TO   KHJ-F13.
     MOVE     KHS-F06        TO   KHJ-F06.
     MOVE     KHS-F07        TO   KHJ-F07.
     READ     KHJOHOF    INVALID
              MOVE    "INV"       TO   KHJOHOF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   KHJOHOF-INV-FLG
     END-READ.
*
 KHJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　欠品区分更新処理
****************************************************************
 LSTNMEF-REWRITE-SEC SECTION.
*
     MOVE     SPACE          TO   LST-REC.
     INITIALIZE                   LST-REC.
     MOVE     WK-LST-F01     TO   LST-F01.
     MOVE     WK-LST-F02     TO   LST-F02.
     MOVE     WK-LST-F03     TO   LST-F03.
     MOVE     WK-LST-F04     TO   LST-F04.
     MOVE     WK-LST-F05     TO   LST-F05.
     MOVE     WK-LST-F07     TO   LST-F07.
     READ     LSTNMEF    INVALID
              MOVE    "INV"       TO   LSTNMEF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   LSTNMEF-INV-FLG
     END-READ.
*
     IF  LSTNMEF-INV-FLG = "INV"
         DISPLAY NC"＃　ＦＬレコード更新異常！！" UPON CONS
         MOVE    4000             TO   PROGRAM-STATUS
     ELSE
         MOVE    "1"              TO   LST-F10(26:1)
         REWRITE  LST-REC
     END-IF.
*
     MOVE         SPACE           TO   KEP-FLG.
*
 LSTNMEF-REWRITE-EXIT.
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
