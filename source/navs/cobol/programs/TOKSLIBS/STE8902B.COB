****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　リック　　　　　　ＷＥＢＥＤＩ　　*
*    業務名　　　　　　　：　リック　　　　　　ＷＥＢＥＤＩ　　*
*    モジュール名　　　　：　納品予定データ作成                *
*    作成日／更新日　　　：　2008/04/21                        *
*    作成者／更新者　　　：　NAV SATO                          *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
*    更新日／更新者　　　：　2011/10/06 / YOSHIDA.M            *
*    修正概要　　　　　　：　基幹サーバ統合                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STE8902B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/21.
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
***2011.10.06(DEN-F07,DEN-F112)
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*リック　出荷情報ファイル
     SELECT   RCSYUKF   ASSIGN    TO        DA-01-VI-RCSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RCS-K01   RCS-K02
                                            RCS-K03   RCS-K04
                                            RCS-K05   RCS-K06
                                            RCS-K07   RCS-K08
                        FILE      STATUS    RCS-STATUS.
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
*    リック　出荷情報データ
******************************************************************
 FD  RCSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     RCSYUKF   OF        XFDLIB
              JOINING   RCS       PREFIX.
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
*
*----<< 店舗マスタ >>--*
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
     03  RCSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  RCJ-STATUS        PIC  X(02).
     03  RCS-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STE8902B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE8902B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE8902B".
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
*01  PARA-JDATE             PIC   9(08).
*01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-DENST             PIC   9(09).
 01  PARA-DENED             PIC   9(09).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
*PROCEDURE              DIVISION USING PARA-JDATE
*                                      PARA-JTIME
 PROCEDURE              DIVISION USING PARA-TORICD
                                       PARA-DENST
                                       PARA-DENED
                                       PARA-SOKO
                                       PARA-NOUDT.
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
                        PROCEDURE   RCSYUKF.
     MOVE      "RCSYUKL1 "   TO   AB-FILE.
     MOVE      RCS-STATUS   TO   AB-STS.
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
     OPEN     I-O       RCSYUKF.
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
     MOVE      PARA-DENST     TO   DEN-F02
     START     SHTDENF   KEY  >=   DEN-F01   DEN-F02
                                   DEN-F04   DEN-F051
***2011.10.06(DEN-F07,DEN-F112)
                                   DEN-F07   DEN-F112
                                   DEN-F03
          INVALID   KEY
              DISPLAY "AAA" UPON CONS
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    リック　売上伝票データ読込み
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
*    伝票番号のチェック
     IF       PARA-DENST   =  ZERO
              CONTINUE
     ELSE
*             DISPLAY "PARA-DENST=" PARA-DENST UPON CONS
*             DISPLAY "PARA-DENED=" PARA-DENED UPON CONS
*             DISPLAY "DEN-F02   =" DEN-F02    UPON CONS
              IF   PARA-DENST   <=  DEN-F02    AND
                   DEN-F02      <=  PARA-DENED
***                DISPLAY "GGG"   UPON CONS
                   CONTINUE
              ELSE
***                DISPLAY "FFF"   UPON CONS
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  SHTDENF-READ-EXIT
              END-IF
     END-IF.
*オンラインの場合は対象外とする。
     IF       DEN-F274  NOT =  ZERO
              GO                 TO   SHTDENF-READ-SEC
     END-IF.
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
*    DEN-F03=80 YOMITOBASI
     IF       DEN-F03 = 80
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
*    リック 出荷情報データWRITE
     PERFORM   RCSYUKF-WRITE-SEC.
*
*    リック 売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　リック　発注確定データ作成処理
****************************************************************
 RCSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "RCSYUKF-WRITE-SEC"  TO  S-NAME.
*初期化
     MOVE      SPACE               TO  RCS-REC.
     INITIALIZE                        RCS-REC.
*存在チェックキーセット
     MOVE      99999999            TO  RCS-K01.
     MOVE      9999                TO  RCS-K02.
     DISPLAY "DEN-F01=" DEN-F01 ":DEN-02=" DEN-F02 UPON CONS.
     MOVE      DEN-F01             TO  RCS-K03.
     MOVE      DEN-F09             TO  RCS-K04.
     MOVE      DEN-F07             TO  RCS-K05.
     MOVE      DEN-F02             TO  RCS-K06.
     MOVE      DEN-F03             TO  RCS-K07.
     MOVE      DEN-F112            TO  RCS-K08.
**存在チェック
     READ      RCSYUKF
               INVALID
               MOVE   "INV"        TO  RCSYUKF-INV-FLG
               NOT  INVALID
               MOVE   SPACE        TO  RCSYUKF-INV-FLG
     END-READ.
 CHK010.
*判定
     IF        RCSYUKF-INV-FLG = SPACE
               GO                  TO  RCSYUKF-WRITE-EXIT
     END-IF.
*共通エリア
 CHK020.
*初期化
     MOVE      SPACE               TO  RCS-REC.
     INITIALIZE                        RCS-REC.
**バッチ日付
     MOVE      99999999            TO  RCS-K01.
**バッチ時刻
     MOVE      9999                TO  RCS-K02.
**バッチ取引先
     MOVE      DEN-F01             TO  RCS-K03.
**倉庫コード　
     MOVE      DEN-F09             TO  RCS-K04.
**店舗コード　
     MOVE      DEN-F07             TO  RCS-K05.
**伝票番号
     MOVE      DEN-F02             TO  RCS-K06.
**行番号　
     MOVE      DEN-F03             TO  RCS-K07.
**納品日　
*****MOVE      PARA-NOUDT          TO  RCS-K08.
     MOVE      DEN-F112            TO  RCS-K08.
*出荷情報データヘッダ作成
**タグ
     MOVE      "HD"                TO  RCS-F01.
**伝票番号　　
     MOVE      DEN-F02(3:7)        TO  RCS-F02.
**発注日
     MOVE      DEN-F111            TO  RCS-F03.
**納品日
     MOVE      DEN-F112            TO  RCS-F04.
**伝票タイプ
     MOVE      "21"                TO  RCS-F05.
**伝票区分　　
     MOVE      "20"                TO  RCS-F06.
**ルート
     MOVE      "00"                TO  RCS-F07.
**小売企業コード
     MOVE      "00000000"          TO  RCS-F08.
**小売企業名称（カナ）
     MOVE      "(ｶ)ﾘｯｸｺｰﾎﾟﾚｰｼｮﾝ"   TO  RCS-F09.
**小売企業名称（漢字）
     MOVE      NC"株式会社リックコーポレーション"  TO  RCS-F102.
**部門名称（カナ）
**   MOVE      ********            TO  RCS-F11.
**部門名称（漢字）
**   MOVE      ********            TO  RCS-F12.
**部門コード
*****MOVE      DEN-F12             TO  RCS-F13.
     EVALUATE  DEN-F01
       WHEN  1994
         MOVE  "ｴﾝｹﾞｲ"                            TO RCS-F11
         MOVE  NC"園芸　　　　　　　　　　　　　" TO RCS-F122
         MOVE  "2102"                             TO RCS-F13
       WHEN  1995
         MOVE  "ﾊﾅ"                               TO RCS-F11
         MOVE  NC"花　　　　　　　　　　　　　　" TO RCS-F122
         MOVE  "2104"                             TO RCS-F13
     END-EVALUATE.
**店舗コード
     MOVE      DEN-F07(2:4)        TO  RCS-F14.
     MOVE      DEN-F07             TO  TEN-F011.
**仕入先コード
     MOVE      DEN-F01(3:6)        TO  RCS-F15  TEN-F52.
**仕入先名称（カナ）
     MOVE      "(ｶ)ｻｶﾀﾉﾀﾈ"         TO  RCS-F16.
**仕入先名称（漢字）
     MOVE      NC"株式会社サカタのタネ"   TO  RCS-F172.
     READ     HTENMS    INVALID
                    MOVE      SPACE     TO   TEN-F02
     END-READ.
**店舗名（カナ)
     MOVE      TEN-F04             TO  RCS-F18.
**店舗名（漢字）
     MOVE      TEN-F02             TO  RCS-F192.
**送信先コード
     MOVE      "00199400"          TO  RCS-F20.
**仕入先ＴＥＬ
     MOVE      "045-945-8816"      TO  RCS-F21.
**タグ　
     MOVE      "DT"                TO  RCS-M01.
**伝票行番号
     MOVE      DEN-F03             TO  RCS-M03.
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
                      MOVE    SPACE           TO  RCS-M052
                      MOVE    SPACE           TO  RCS-M072
                      NOT INVALID
                      MOVE    MEI-F021        TO  RCS-M052
                      MOVE    MEI-F022        TO  RCS-M072
              END-READ
*             ＪＡＮ
              MOVE      "9999999999999"     TO  RCS-M02
*             発注単位数
              MOVE      1                   TO  RCS-M13
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
              MOVE        TBL-F031  TO        MEI-F011
              MOVE        TBL-F032  TO        MEI-F012
              READ    MEIMS1
                      NOT INVALID
                      MOVE    MEI-F021          TO  RCS-M052
                      MOVE    MEI-F022          TO  RCS-M072
              END-READ
*             ＪＡＮ
              MOVE      DEN-F25             TO  RCS-M02
*             発注単位数
              MOVE      TBL-F11             TO  RCS-M13
     END-IF.
*
**商品名（カナ)
     MOVE      DEN-F1421           TO  RCS-M04.
**規格名（カナ）
     MOVE      DEN-F1422           TO  RCS-M06.
**単位
     MOVE      SPACE               TO  RCS-M08.
**欠品理由
     MOVE      SPACE               TO  RCS-M09.
**発注数量
     MOVE      DEN-F15             TO  RCS-M10.
**入数　
     MOVE      1                   TO  RCS-M11.
**納品数量
     MOVE      DEN-F15             TO  RCS-M12.
**原価金額
     COMPUTE   RCS-M14  =  DEN-F15  *  DEN-F172.
**売価金額
     COMPUTE   RCS-M15  =  DEN-F15  *  DEN-F173.
**原単価
*****MOVE      DEN-F172            TO  RCS-M16.
     COMPUTE RCS-M16 = DEN-F172 * 100.
**売単価
     MOVE      DEN-F173            TO  RCS-M17.
*漢字制御バイト転送
     MOVE      X"28"   TO  RCS-F101 RCS-F121 RCS-F171 RCS-F191
                           RCS-M051 RCS-M071.
     MOVE      X"29"   TO  RCS-F103 RCS-F123 RCS-F173 RCS-F193
                           RCS-M053 RCS-M073.
*更新
     WRITE     RCS-REC.
     ADD       1                   TO  KOS1-CNT.
*
 RCSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ﾘｯｸ  F      READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ       CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "ﾘｯｸ  ｶｸﾃｲDT WT   CNT = " KOS1-CNT  UPON CONS.
*
     CLOSE     SHTDENF  RCSYUKF  SHOTBL1  MEIMS1  HTENMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
