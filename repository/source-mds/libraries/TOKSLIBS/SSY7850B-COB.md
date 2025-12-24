# SSY7850B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7850B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　ロイヤルＨＣ新ＥＤＩ対応
*    業務名　　　　　　　：　ロイヤルＨＣ新ＥＤＩ対応
*    モジュール名　　　　：　納品予定データ作成
*    作成日／更新日　　　：　2008/10/27
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　受け取ったパラメタより対象データ
*                            を出荷情報保存データより抽出する。
*    更新履歴            ：
*      2011/10/07 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7850B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/10/27.
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
*ロイヤルＨＣ　発注情報保存データ
     SELECT   RHJOHOF   ASSIGN    TO        DA-01-VI-RHJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RHJ-F01   RHJ-F02
                                            RHJ-F03   RHJ-F04
                                            RHJ-F08   RHJ-F05
                                            RHJ-F06   RHJ-F07
                        FILE      STATUS    RHJ-STATUS.
*ロイヤルＨＣ　出荷情報修正
     SELECT   RHSYUKF   ASSIGN    TO        DA-01-VI-RHSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RHS-F01   RHS-F02
                                            RHS-F03   RHS-F04
                                            RHS-F05   RHS-F06
                                            RHS-F07   RHS-F08
                        FILE      STATUS    RHS-STATUS.
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
*    ロイヤルＨＣ　　　　　発注情報保存データ
******************************************************************
 FD  RHJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     RHJOHOF   OF        XFDLIB
              JOINING   RHJ       PREFIX.
******************************************************************
*    ロイヤルＨＣ　　　　　発注データ
******************************************************************
 FD  RHSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     RHSYUKF   OF        XFDLIB
              JOINING   RHS       PREFIX.
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
     03  RHSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
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
     03  RHJ-STATUS        PIC  X(02).
     03  RHS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7850B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7850B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7850B".
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
 01  WK-C25.
     03  WK-C251           PIC  9(05)   VALUE  ZERO.
     03  WK-C252           PIC  X(02)   VALUE  SPACE.
     03  WK-C253           PIC  X(02)   VALUE  SPACE.
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
                        PROCEDURE   RHJOHOF.
     MOVE      "RHJOHOL2 "   TO   AB-FILE.
     MOVE      RHJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RHSYUKF.
     MOVE      "RHSYUKL5 "   TO   AB-FILE.
     MOVE      RHS-STATUS   TO   AB-STS.
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
     OPEN     I-O       RHJOHOF  SHTDENF.
     OPEN     I-O       RHSYUKF.
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
*    ロイヤルＨＣ　　　　　発注情報保存データスタート
     MOVE     SPACE          TO   RHJ-REC.
     INITIALIZE                   RHJ-REC.
     MOVE     PARA-JDATE     TO   RHJ-F01.
     MOVE     PARA-JTIME     TO   RHJ-F02.
     MOVE     PARA-TORICD    TO   RHJ-F03.
     MOVE     PARA-SOKO      TO   RHJ-F04.
     MOVE     PARA-NOUDT     TO   RHJ-F08.
     MOVE     ZERO           TO   RHJ-F05.
     MOVE     ZERO           TO   RHJ-F06.
     MOVE     ZERO           TO   RHJ-F07.
     START    RHJOHOF   KEY  >=   RHJ-F01   RHJ-F02
                                  RHJ-F03   RHJ-F04
                                  RHJ-F08   RHJ-F05
                                  RHJ-F06   RHJ-F07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ロイヤルＨＣ　　　　　発注情報保存データ読込み
     PERFORM RHJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RHJOHOF-READ-SEC    SECTION.
*
     READ     RHJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  RHJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  RHJ-F01
     AND      PARA-JTIME  =  RHJ-F02
     AND      PARA-TORICD =  RHJ-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  RHJOHOF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  RHJ-F03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  RHJOHOF-READ-EXIT
              END-IF
     END-IF.
*    送信ＦＬＧのチェック
     IF       RHJ-F11  =  "1"
              GO                 TO   RHJOHOF-READ-SEC
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  RHJ-F04
                   CONTINUE
              ELSE
                   GO            TO  RHJOHOF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  RHJ-F08
                  CONTINUE
              ELSE
                  GO        TO   RHJOHOF-READ-SEC
              END-IF
     END-IF.
*
 RHJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     RHJ-F03             TO   DEN-F01.
     MOVE     RHJ-F06             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     RHJ-F07             TO   DEN-F03.
* 2011/10/07,S  S.I/NAV
     MOVE  RHJ-F05          TO  DEN-F07.  *> 店舗CD
     MOVE  RHJ-F08          TO  DEN-F112. *> 納品日
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
              PERFORM   RHSYUKF-READ-SEC
              IF    RHSYUKF-INV-FLG = "INV"
                    PERFORM   RHSYUKF-WRITE-SEC
                    MOVE   "1"       TO   RHJ-F09
                    MOVE  SYS-DATEW  TO   RHJ-F10
                    REWRITE  RHJ-REC
              END-IF
     END-IF.
 MAIN010.
*    ロイヤルＨＣ 発注情報保存データ読込み
     PERFORM RHJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ロイヤルＨＣ　発注確定データ作成処理
****************************************************************
 RHSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "RHSYUKF-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  RHS-REC.
     INITIALIZE                        RHS-REC.
     MOVE      RHJ-REC             TO  RHS-REC.
*    予備／ＲＳＥＱ
     MOVE      SPACE               TO  RHS-B12  RHS-B13  RHS-B14.
     MOVE      SPACE               TO  RHS-C26  RHS-C27.
     MOVE      SPACE               TO  RHS-C24.
*    取引先商品コード
     MOVE      SPACE               TO  RHS-C08.
*    単価　原価　各区分
     MOVE      ZERO                TO  RHS-C10 RHS-C12 RHS-C14
                                       RHS-C16.
*    出荷数量
     MOVE      DEN-F15             TO  RHS-C251.
*    出荷数量が発注数量と異なる場合
     IF        DEN-F15  NOT =  DEN-F50
*              訂正事由
               MOVE      "03"                TO  RHS-C252
*              予備
               MOVE      SPACE               TO  RHS-C253
*              原価金額
               COMPUTE   RHS-C15  =  RHS-C11 * RHS-C251
*              売価金額
               COMPUTE   RHS-C17  =  RHS-C13 * RHS-C251
     ELSE
               MOVE      SPACE               TO  RHS-C252 RHS-C253
     END-IF.
*
     WRITE     RHS-REC.
     ADD       1                   TO  KOS1-CNT.
*
 RHSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ﾛｲﾔﾙ F      READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾛｲﾔﾙ DT ﾅｼ  CNT      = " SKIP2-CNT UPON CONS.
     DISPLAY "ﾛｲﾔﾙ ｶｸﾃｲDT WT   CNT = " KOS1-CNT  UPON CONS.
*
     CLOSE     SHTDENF  RHJOHOF  RHSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RHSYUKF-READ-SEC    SECTION.
*
     MOVE     SPACE          TO   RHS-REC.
     INITIALIZE                   RHS-REC.
     MOVE     RHJ-F01        TO   RHS-F01.
     MOVE     RHJ-F02        TO   RHS-F02.
     MOVE     RHJ-F03        TO   RHS-F03.
     MOVE     RHJ-F04        TO   RHS-F04.
     MOVE     RHJ-F08        TO   RHS-F08.
     MOVE     RHJ-F05        TO   RHS-F05.
     MOVE     RHJ-F06        TO   RHS-F06.
     MOVE     RHJ-F07        TO   RHS-F07.
     MOVE     RHJ-F08        TO   RHS-F08.
     READ     RHSYUKF    INVALID
              MOVE    "INV"       TO   RHSYUKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   RHSYUKF-INV-FLG
     END-READ.
*
 RHSYUKF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
