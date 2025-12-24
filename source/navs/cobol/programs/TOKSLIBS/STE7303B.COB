****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト新ＥＤＩシステム　　*
*    業務名　　　　　　　：　ダイユーエイト新ＥＤＩシステム　　*
*    モジュール名　　　　：　手書納品予定データ作成            *
*    作成日／更新日　　　：　2010/07/23                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
*    作成日／更新日　　　：　2013/09/10                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　日敷追加　　　　　　　　　　　　  *
*    作成日／更新日　　　：　2018/01/29                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　リック資材／植物追加　　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STE7303B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/07/23.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*出荷情報ファイル
     SELECT   DYSYUKF   ASSIGN    TO        DA-01-VI-DYSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DYS-F01   DYS-F02
                                            DYS-F03   DYS-F04
                                            DYS-F05   DYS-F06
                                            DYS-F07   DYS-F08
                        FILE      STATUS    DYS-STATUS.
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
*店舗マスタ
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    出荷情報データ
******************************************************************
 FD  DYSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DYSYUKF   OF        XFDLIB
              JOINING   DYS       PREFIX.
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
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP1-CNT           PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  KOS1-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  DYSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  WK-NOUSU                PIC  9(08)     VALUE  ZERO.
 01  WK-HAISIN.
     03  WK-HACHSHA          PIC  9(04)     VALUE  ZERO.
     03  WK-TORICD           PIC  9(06)     VALUE  ZERO.
     03  FILLER              PIC  X(03)     VALUE  SPACE.
 01  WK-HAISINR     REDEFINES     WK-HAISIN.
     03  WK-HAISINCD         PIC  X(13).
*
*システム日付の編集
 01  WK-HACSU                PIC 9(11)      VALUE  ZERO.
 01  WK-GENTAN               PIC 9(13)      VALUE  ZERO.
 01  WK-GENKIN               PIC 9(13)      VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
     03  SYS-TIME          PIC 9(08).
 01  SYS-TIMEW.
     03  SYS-TIMEHM        PIC  9(04).
     03  FILER             PIC  9(04).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  RCJ-STATUS        PIC  X(02).
     03  DYS-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STE7303B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE7303B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE7303B".
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
*納品予定数量
 01  WK-NOUHIN.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
     03  NOU-F01           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
*****03  NOU-F02           PIC  9(09)   VALUE  ZERO.
     03  NOU-F02           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(11)   VALUE  SPACE.
*納品予定原価金額
 01  WK-GENKA.
     03  GEN-F01           PIC  9(09)   VALUE  ZERO.
*****03  FILLER            PIC  X(02)   VALUE  SPACE.
*納品予定原価金額
 01  WK-BAIKA              PIC  9(09)   VALUE  ZERO.
*納品予定日項目セット
 01  WK-A05.
     03  WK-A051           PIC  X(04)   VALUE  SPACE.
     03  WK-A052           PIC  9(08)   VALUE  ZERO.
     03  WK-A053           PIC  9(08)   VALUE  ZERO.
     03  WK-A054           PIC  X(24)   VALUE  ZERO.
     03  WK-A055           PIC  X(09)   VALUE  ZERO.
     03  WK-A056           PIC  X(230)  VALUE  ZERO.
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
 01  PARA-NOUDT             PIC   9(08).
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
                                       PARA-NOUDT
                                       PARA-DENST
                                       PARA-DENED.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYSYUKF.
     MOVE      "DYSYUKL1 "   TO   AB-FILE.
     MOVE      DYS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
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
 FILEERR-SEC4           SECTION.
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
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     MOVE      "HTENMS"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
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
     OPEN     I-O       SHTDENF.
     OPEN     I-O       DYSYUKF.
     OPEN     INPUT     SHOTBL1.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     HTENMS.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     ACCEPT      SYS-TIME  FROM      TIME.
     MOVE        SYS-TIME  TO        SYS-TIMEW.
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
*    売上伝票データスタート
     MOVE      SPACE          TO   DEN-REC
     INITIALIZE                    DEN-REC
     MOVE      PARA-TORICD    TO   DEN-F01
*    MOVE      010000000      TO   DEN-F02
     MOVE      PARA-DENST     TO   DEN-F02
     START     SHTDENF   KEY  >=   DEN-F01   DEN-F02
                                   DEN-F04   DEN-F051
                                   DEN-F07   DEN-F112
                                   DEN-F03
          INVALID   KEY
              DISPLAY "AAA" UPON CONS
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読込　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC    SECTION.
*
     READ     SHTDENF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  SHTDENF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  DEN-F01
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  SHTDENF-READ-EXIT
              END-IF
     END-IF.
 READ000.
*オンラインの場合は対象外とする。
     IF       DEN-F274  NOT =  ZERO
              GO                 TO   SHTDENF-READ-SEC
     END-IF.
 READ001.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  DEN-F09
                   CONTINUE
              ELSE
**                 DISPLAY "HHH"   UPON CONS
                   GO            TO  SHTDENF-READ-SEC
              END-IF
     END-IF.
 READ002.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  DEN-F112
                  CONTINUE
              ELSE
**                DISPLAY "III" UPON CONS
                  GO        TO   SHTDENF-READ-SEC
              END-IF
     END-IF.
 READ003.
*    伝票番号の範囲チェック
     IF       PARA-DENST  <=  DEN-F02
     AND      PARA-DENED  >=  DEN-F02
              CONTINUE
     ELSE
              GO        TO   SHTDENF-READ-SEC
     END-IF.
 READ004.
*    伝票番号の８０行目は対象外
     IF       DEN-F03  NOT =  80
              CONTINUE
     ELSE
              GO        TO   SHTDENF-READ-SEC
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*MAIN010.
*    出荷情報データWRITE
     PERFORM   DYSYUKF-WRITE-SEC.
*
*    売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　発注確定データ作成処理
****************************************************************
 DYSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "DYSYUKF-WRITE-SEC"  TO  S-NAME.
*初期化
     MOVE      SPACE               TO  DYS-REC.
     INITIALIZE                        DYS-REC.
*存在チェックキーセット
     MOVE      99999999            TO  DYS-F01.
     MOVE      9999                TO  DYS-F02.
*    DISPLAY "DEN-F01=" DEN-F01 ":DEN-02=" DEN-F02 UPON CONS.
     MOVE      DEN-F01             TO  DYS-F03.
     MOVE      DEN-F09             TO  DYS-F04.
     MOVE      DEN-F07             TO  DYS-F05.
     MOVE      DEN-F02             TO  DYS-F06.
     MOVE      DEN-F03             TO  DYS-F07.
     MOVE      DEN-F112            TO  DYS-F08.
**存在チェック
     READ      DYSYUKF
               INVALID
               MOVE   "INV"        TO  DYSYUKF-INV-FLG
               NOT  INVALID
               MOVE   SPACE        TO  DYSYUKF-INV-FLG
     END-READ.
 CHK010.
*判定
     IF        DYSYUKF-INV-FLG = SPACE
               GO                  TO  DYSYUKF-WRITE-EXIT
     END-IF.
*共通エリア
 CHK020.
*初期化
     MOVE      SPACE               TO  DYS-REC.
     INITIALIZE                        DYS-REC.
**バッチ日付
     MOVE      99999999            TO  DYS-F01.
**バッチ時刻
     MOVE      9999                TO  DYS-F02.
**バッチ取引先
     EVALUATE  DEN-F01
       WHEN  1225
         MOVE  DEN-F01             TO  WK-TORICD
         MOVE  DEN-F01             TO  DYS-F03
       WHEN  12251
         MOVE  DEN-F01             TO  DYS-F03
         MOVE  1225                TO  WK-TORICD
*      2013.09.10↓
       WHEN  12252
         MOVE  DEN-F01             TO  DYS-F03
         MOVE  1225                TO  WK-TORICD
*      2013.09.10↑
*      2018/01/29 NAV ST↓
       WHEN  1994
         MOVE  DEN-F01             TO  DYS-F03
         MOVE  DEN-F01             TO  WK-TORICD
       WHEN  1995
         MOVE  DEN-F01             TO  DYS-F03
         MOVE  DEN-F01             TO  WK-TORICD
*      2018/01/29 NAV ED↑
     END-EVALUATE.
**倉庫コード　
     MOVE      DEN-F09             TO  DYS-F04.
**店舗コード　
     MOVE      DEN-F07             TO  DYS-F05.
**伝票番号
     MOVE      DEN-F02             TO  DYS-F06.
**行番号　
     MOVE      DEN-F03             TO  DYS-F07.
**納品日　
     MOVE      DEN-F112            TO  DYS-F08.
**納品予定作成区分
**** MOVE      SPACE               TO  DYS-F09.
     MOVE      1                   TO  DYS-F09.
**納品予定作成日付
     MOVE      SYS-DATEW           TO  DYS-F10.
**納品予定送信区分
     MOVE      SPACE               TO  DYS-F11.
**納品予定送信日付
     MOVE      ZERO                TO  DYS-F12.
**分類ＣＤ
     MOVE      DEN-F12             TO  DYS-F13.
**伝票区分
**** MOVE      DEN-F05             TO  DYS-F09.
     MOVE      08                  TO  DYS-F14.
**発注日
     MOVE      DEN-F111            TO  DYS-F15.
**納品日
     MOVE      DEN-F112            TO  DYS-F16.
**予備
     MOVE      SPACE               TO  DYS-FIL1.
*出荷情報データヘッダ作成
**タグ
     MOVE      "HD"                TO  DYS-H01.
**ＥＤＩＦＡＣＴメッセージ
     MOVE      "ORDERS"            TO  DYS-H02.
**発注伝票番号　　
     MOVE      DEN-F02             TO  DYS-H03.
**取引先コード
*****MOVE      DYS-F03             TO  DYS-H04.
     MOVE      WK-TORICD           TO  DYS-H04.
**データ作成年月日
     MOVE      DEN-F111            TO  DYS-H05.
**発注年月日
     MOVE      DEN-F111            TO  DYS-H06.
**** MOVE      SPACE               TO  DYS-H06A.
**データ作成時刻
     MOVE      SYS-TIMEHM          TO  DYS-H07.
**納品指定年月日
     MOVE      DEN-F112            TO  DYS-H08.
**** MOVE      SPACE               TO  DYS-H08X,
**納品予定年月日
     MOVE      DEN-F112            TO  DYS-H08A.
**摘要１
     MOVE      SPACE               TO  DYS-H09.
**摘要２
     MOVE      SPACE               TO  DYS-H10.
**ＥＯＳ入力区分
     MOVE      "2"                 TO  DYS-H10A.
**伝票区分
     MOVE      DEN-F132            TO  DYS-H11.
**店別・総量区分
     MOVE      "1"                 TO  DYS-H12.
**発注社コード
     EVALUATE  DEN-F01
       WHEN  1225
         MOVE  "4950"              TO  DYS-H13
         MOVE  "4950"              TO  WK-HACHSHA
       WHEN  12251
         MOVE  "4952"              TO  DYS-H13
         MOVE  "4952"              TO  WK-HACHSHA
*      2013.09.10↓
       WHEN  12252
         MOVE  "4953"              TO  DYS-H13
         MOVE  "4953"              TO  WK-HACHSHA
*      2013.09.10↑
*      2018/01/29 NAV ST↓
       WHEN  1994   WHEN   1995
         MOVE  "4978"              TO  DYS-H13
         MOVE  "4978"              TO  WK-HACHSHA
*      2018/01/29 NAV ED↑
     END-EVALUATE.
**発注社名（カナ）
     MOVE      "(ｶ)ﾀﾞｲﾕｰｴｲﾄ"       TO  DYS-H14.
**漢字発注社名
     MOVE      NC"株式会社ダイユーエイト"  TO  DYS-H15.
**2013/09/11 NAV ST 日敷の名称を追加
     IF  DEN-F01 = 12252
**発注社名（カナ）
         MOVE      "ｶﾌﾞｼｷｶﾞｲｼﾔ ﾆﾂｼｷ"   TO  DYS-H14
**漢字発注社名
         MOVE      NC"株式会社　日敷"  TO  DYS-H15
     END-IF.
*#2018/01/29 NAV ST リック名称追加
     IF  DEN-F01 = 1994  OR  1995
**       発注社名（カナ）
         MOVE      "ﾘﾂｸｺ-ﾎﾟﾚ-ｼﾖﾝ"      TO  DYS-H14
**       漢字発注社名
         MOVE NC"（株）リックコーポレーション" TO  DYS-H15
     END-IF.
*#2018/01/29 NAV ED リック名称追加
**分類ＣＤ
     MOVE      DEN-F12             TO  DYS-H16.
**店舗ＣＤ
     MOVE      DEN-F07             TO  DYS-H17.
**店名（カナ）
     MOVE      DEN-F30             TO  DYS-H18.
**店舗マスタ検索
     MOVE        DEN-F01   TO        TEN-F52.
     MOVE        DEN-F07   TO        TEN-F011.
              READ    HTENMS
                      INVALID
                      MOVE    SPACE         TO  DYS-H19
                      NOT INVALID
**漢字店名
                      MOVE    TEN-F03       TO  DYS-H19
              END-READ.
**配信先コード
     MOVE      WK-HAISINCD         TO  DYS-H20.
**取引先名（カナ）
     MOVE      "(ｶ)ｻｶﾀﾉﾀﾈ"         TO  DYS-H21.
**漢字取引先名
     MOVE      NC"株式会社サカタのタネ"  TO  DYS-H22.
**配送センターコード
     MOVE   ALL  ZERO              TO  DYS-H23.
*出荷情報データ明細作成
**タグ　
     MOVE      "DT"                TO  DYS-M01.
**伝票行番号
     MOVE      DEN-F03             TO  DYS-M03.
**システム伝票番号
     MOVE      DEN-F02             TO  DYS-M04.
*  商品変換テーブル検索
*       ＜相手ＣＤがないものは対象としない＞
     IF      DEN-F25  =  SPACE
             DISPLAY NC"相手商品ＣＤなし" UPON CONS
*             商品名称マスタ検索
              MOVE        SPACE     TO        MEI-REC
              INITIALIZE                      MEI-REC
              MOVE        DEN-F1411 TO        MEI-F011
              MOVE        DEN-F1412 TO        MEI-F012
              READ    MEIMS1
                      INVALID
                      MOVE    SPACE         TO  DYS-M05
                      MOVE    SPACE         TO  DYS-M06
                      NOT INVALID
                      MOVE    MEI-F03       TO  DYS-M05
                      MOVE    MEI-F02       TO  DYS-M06
              END-READ
*             ＪＡＮ
*             MOVE      "9999999999999"     TO  DYS-M02
*             発注単位数
*             MOVE      1                   TO  DYS-M13
     ELSE
             MOVE    DEN-F01       TO        TBL-F01
             MOVE    DEN-F25       TO        TBL-F02
             READ    SHOTBL1
                     INVALID
                     MOVE    SPACE     TO        TBL-REC
                     INITIALIZE                  TBL-REC
              END-READ
*             商品名称マスタ検索
              MOVE        SPACE     TO        MEI-REC
              INITIALIZE                      MEI-REC
              MOVE        DEN-F1411 TO        MEI-F011
              MOVE        DEN-F1412 TO        MEI-F012
              READ    MEIMS1
                      NOT INVALID
                      MOVE    MEI-F03       TO  DYS-M05
                      MOVE    MEI-F02       TO  DYS-M06
              END-READ
*             標準商品ＣＤ
*******       MOVE      TBL-F031            TO  DYS-M02
              MOVE      TBL-F02             TO  DYS-M02
*             ＪＡＮ
*             MOVE      DEN-F25             TO  DYS-M02
*             発注単位数（ケース）
***********   MOVE      TBL-F0321           TO  DYS-M08
              MOVE      10                  TO  DYS-M08
     END-IF.
*
**発注数量
**** MOVE      DEN-F15             TO  DYS-M07.
     COMPUTE   WK-HACSU  =   DEN-F15  *  100.
     MOVE      WK-HACSU            TO  DYS-M07.
**入数　
     MOVE      0                   TO  DYS-M09.
**納品数量
*****MOVE      DEN-F15             TO  DYS-M09A.
     COMPUTE   WK-NOUSU    =   DEN-F15  *  10.
     MOVE      WK-NOUSU            TO  DYS-M09A.
**原価金額
**** MOVE      DEN-F181            TO  DYS-M10.
     COMPUTE   WK-GENKIN   =  DEN-F181  * 100.
     MOVE      WK-GENKIN           TO  DYS-M10.
**売価金額
     MOVE      DEN-F182            TO  DYS-M11.
**原単価
*****MOVE      DEN-F172            TO  DYS-M12.
     COMPUTE   WK-GENTAN   =   DEN-F172 *  100.
     MOVE      WK-GENTAN           TO  DYS-M12.
**売単価
     MOVE      DEN-F173            TO  DYS-M13.
**システム伝票行番号
     MOVE      DEN-F03             TO  DYS-M14.
**理由区分
*****IF        DEN-F411 = SPACE
*******MOVE   "00"                 TO  DYS-M14A
*****ELSE
*******MOVE   "01"                 TO  DYS-M14A
*****END-IF.
     MOVE     "00"                 TO  DYS-M14A.
**固定値
     MOVE      "00000"             TO  DYS-M15.
*漢字制御バイト転送
     MOVE      X"28"   TO  DYS-H15S DYS-H19S DYS-H22S DYS-M06S.
     MOVE      X"29"   TO  DYS-H15E DYS-H19E DYS-H22E DYS-M06E.
*更新
     WRITE     DYS-REC.
     ADD       1                   TO  KOS1-CNT.
*
 DYSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ｳﾘｱｹﾞF       READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ        CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "ｼｭｯｶ  ｶｸﾃｲDT WT   CNT = " KOS1-CNT  UPON CONS.
*
     CLOSE     SHTDENF  DYSYUKF  SHOTBL1  MEIMS1  HTENMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
