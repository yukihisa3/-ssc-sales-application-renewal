# SSY8723B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8723B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　*
*    モジュール名　　　　：　納品予定データ作成                *
*    作成日／更新日　　　：　2007/05/17                        *
*    作成者／更新者　　　：　NAV MATSUNO                       *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
*    更新履歴            ：
*      2011/10/07 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8723B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/17.
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
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                       *> 2011/10/07,S  S.I/NAV
                                            DEN-F07   DEN-F112
                                       *> 2011/10/07,E  S.I/NAV
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*ＤＣＭＪＡＰＡＮ発注情報保存データ
     SELECT   DJJOHOF   ASSIGN    TO        DA-01-VI-DJJOHOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DJJ-K01   DJJ-K02
                                            DJJ-K03   DJJ-K04
                                            DJJ-K08   DJJ-K05
                                            DJJ-K06   DJJ-K07
                        FILE      STATUS    DJJ-STATUS.
*ＤＣＭＪＡＰＡＮ出荷情報修正
     SELECT   DJSYUKF   ASSIGN    TO        DA-01-VI-DJSYUKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DJS-K01   DJS-K02
                                            DJS-K03   DJS-K04
                                            DJS-K08   DJS-K05
                                            DJS-K06   DJS-K07
                        FILE      STATUS    DJS-STATUS.
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
*    ＤＣＭＪＡＰＡＮ発注情報保存データ
******************************************************************
 FD  DJJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DJJOHOF   OF        XFDLIB
              JOINING   DJJ       PREFIX.
******************************************************************
*    ＤＣＭＪＡＰＡＮ発注データ
******************************************************************
 FD  DJSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DJSYUKF   OF        XFDLIB
              JOINING   DJS       PREFIX.
*
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
     03  DJSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
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
     03  DJJ-STATUS        PIC  X(02).
     03  DJS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8723B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8723B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8723B".
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
     03  FILLER            PIC  X(02)   VALUE  SPACE.
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
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
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
                        PROCEDURE   DJJOHOF.
     MOVE      "DJJOHOL3 "   TO   AB-FILE.
     MOVE      DJJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJSYUKF.
     MOVE      "DJSYUKL3 "   TO   AB-FILE.
     MOVE      DJS-STATUS   TO   AB-STS.
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
     OPEN     I-O       DJJOHOF  SHTDENF.
     OPEN     I-O       DJSYUKF.
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
*    ＤＣＭＪＡＰＡＮ発注情報保存データスタート
     MOVE     SPACE          TO   DJJ-REC.
     INITIALIZE                   DJJ-REC.
     MOVE     PARA-JDATE     TO   DJJ-K01.
     MOVE     PARA-JTIME     TO   DJJ-K02.
     MOVE     PARA-TORICD    TO   DJJ-K03.
     MOVE     PARA-SOKO      TO   DJJ-K04.
     MOVE     PARA-NOUDT     TO   DJJ-K08.
     MOVE     ZERO           TO   DJJ-K05.
     MOVE     ZERO           TO   DJJ-K06.
     MOVE     ZERO           TO   DJJ-K07.
     START    DJJOHOF   KEY  >=   DJJ-K01   DJJ-K02
                                  DJJ-K03   DJJ-K04
                                  DJJ-K08   DJJ-K05
                                  DJJ-K06   DJJ-K07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＤＣＭＪＡＰＡＮ発注情報保存データ読込み
     PERFORM DJJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DJJOHOF-READ-SEC    SECTION.
*
     READ     DJJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DJJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DJJ-K01
     AND      PARA-JTIME  =  DJJ-K02
     AND      PARA-TORICD =  DJJ-K03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  DJJOHOF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  DJJ-K03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  DJJOHOF-READ-EXIT
              END-IF
     END-IF.
*    送信ＦＬＧのチェック
     IF       DJJ-K11  =  "1"
              GO                 TO   DJJOHOF-READ-SEC
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  DJJ-K04
                   CONTINUE
              ELSE
                   GO            TO  DJJOHOF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  DJJ-K08
                  CONTINUE
              ELSE
                  GO        TO   DJJOHOF-READ-SEC
              END-IF
     END-IF.
*
 DJJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     DJJ-K03             TO   DEN-F01.
     MOVE     DJJ-K06             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     DJJ-K07             TO   DEN-F03.
* 2011/10/07,S  S.I/NAV
     MOVE  DJJ-K05          TO  DEN-F07.  *> 店舗CD
     MOVE  DJJ-K08          TO  DEN-F112. *> 納品日
* 2011/10/07,E  S.I/NAV
     READ     SHTDENF    INVALID
              MOVE    "INV"       TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENF-INV-FLG
     END-READ.
*
     IF       SHTDENF-INV-FLG  =  "INV"
              ADD     1           TO   SKIP2-CNT
              GO                  TO   MAIN010
     ELSE
**************数量確認(発注数<>出荷数の時ﾌｧｲﾙに出力)
              PERFORM   DJSYUKF-READ-SEC
              IF    DJSYUKF-INV-FLG = "INV"
                    PERFORM   DJSYUKF-WRITE-SEC
                    MOVE   "1"       TO   DJJ-K09
                    MOVE  SYS-DATEW  TO   DJJ-K10
                    REWRITE  DJJ-REC
              END-IF
     END-IF.
 MAIN010.
*    ＤＣＭＪＡＰＡＮ発注情報保存データ読込み
     PERFORM DJJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ＤＣＭＪＡＰＡＮ発注確定データ作成処理　　　　　　　　　　
****************************************************************
 DJSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "DJSYUKF-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  DJS-REC.
     INITIALIZE                        DJS-REC.
     MOVE      DJJ-REC             TO  DJS-REC.
*    納品日セット（特別処理）
     MOVE      DJS-F042            TO  DJS-K08.
*    メッセージ（発注→納品予定）
     MOVE      "INVOIC"            TO  DJS-F02.
*    伝票番号
     MOVE      DJS-M19             TO  DJS-F03.
*    納品伝票番号
     MOVE      DJS-F03             TO  DJS-M051.
*****宅配区分「00」ｾｯﾄ
*****MOVE      "00"                TO  DJS-F101.
*****MOVE      SPACE               TO  DJS-F101.
*****ホーマック以外は欠品数量ゼロ詰め
     EVALUATE  DJS-K03
         WHEN  880
         WHEN  882
         WHEN  883
         WHEN  1427
         WHEN  14272
         WHEN  14273
*    　　　　出荷数量
             COMPUTE   DJS-M10  =  DEN-F50  -  DEN-F15
         WHEN  OTHER
             MOVE      0000000     TO  DJS-M10
     END-EVALUATE.
*    納品数量
     MOVE     SPACE                TO  WK-NOUHIN.
     INITIALIZE                        WK-NOUHIN.
     MOVE      DEN-F15             TO  NOU-F01.
*****COMPUTE   NOU-F02  =  DEN-F15  *  DJS-M17.
*****MOVE      WK-NOUHIN           TO  DJS-A36  DJJ-A36.
     MOVE      NOU-F01             TO  DJS-A36  DJJ-A36.
*    納品原価金額
     MOVE     SPACE                TO  WK-GENKA.
     INITIALIZE                        WK-GENKA.
     COMPUTE   GEN-F01  =  DEN-F15  *  ( DJS-M16 / 100 ).
     MOVE      WK-GENKA            TO  DJS-A39  DJJ-A39.
*    納品売価金額
     MOVE      ZERO                TO  WK-BAIKA.
     COMPUTE   WK-BAIKA =  DEN-F15  *  DJS-M17.
     MOVE      WK-BAIKA            TO  DJS-A362 DJJ-A362.
*****ホーマック以外は納品原価・売価ゼロ詰め
     EVALUATE  DJS-K03
         WHEN  880
         WHEN  882
         WHEN  883
         WHEN  1427
         WHEN  14272
         WHEN  14273
               CONTINUE
         WHEN  OTHER
               MOVE      000000000   TO  DJS-A39  DJJ-A39
               MOVE      000000000   TO  DJS-A362 DJJ-A362
     END-EVALUATE.
*    欠品区分
     IF        DJS-M10  >  ZERO
               MOVE   10           TO  DJS-M20
     END-IF.
*    納品伝票行番号（発注伝票行番号）
     MOVE      DJS-M03             TO  DJS-A41.
*****納品予定日・出荷日ｾｯﾄ
     INITIALIZE                        WK-A05.
     MOVE      DJS-F06             TO  WK-A053  WK-A052.
     MOVE      WK-A05              TO  DJS-A05.
*    未使用部分には空白をセットする。
     MOVE      SPACE               TO  DJS-A3411.
     MOVE      SPACE               TO  DJS-A35.
     MOVE      SPACE               TO  DJS-A351.
     MOVE      SPACE               TO  DJS-A361.
     MOVE      SPACE               TO  DJS-A363.
     MOVE      SPACE               TO  DJS-A38.
     MOVE      SPACE               TO  DJS-A39(10:2).
     MOVE      SPACE               TO  DJS-A40.
     MOVE      SPACE               TO  DJS-A42.
     MOVE      SPACE               TO  DJS-A43.
*    ダミーセット
     MOVE      "0"                 TO  DJS-A28(48:1).
*
     WRITE     DJS-REC.
     ADD       1                   TO  KOS1-CNT.
*    ADD       1                   TO  KMS-CNT.
*
 DJSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "DCMJAPAN F      READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ           CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "DCMJAPAN ｶｸﾃｲDT WT   CNT = " KOS1-CNT  UPON CONS.
*
     CLOSE     SHTDENF  DJJOHOF  DJSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DJSYUKF-READ-SEC    SECTION.
*
     MOVE     SPACE          TO   DJS-REC.
     INITIALIZE                   DJS-REC.
     MOVE     DJJ-K01        TO   DJS-K01.
     MOVE     DJJ-K02        TO   DJS-K02.
     MOVE     DJJ-K03        TO   DJS-K03.
     MOVE     DJJ-K04        TO   DJS-K04.
     MOVE     DJJ-K08        TO   DJS-K08.
     MOVE     DJJ-K05        TO   DJS-K05.
     MOVE     DJJ-K06        TO   DJS-K06.
     MOVE     DJJ-K07        TO   DJS-K07.
     MOVE     DJJ-K08        TO   DJS-K08.
     READ     DJSYUKF    INVALID
              MOVE    "INV"       TO   DJSYUKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   DJSYUKF-INV-FLG
     END-READ.
*
 DJSYUKF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
