****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト　　ＷＥＢＥＤＩ　　*
*    業務名　　　　　　　：　ダイユーエイト　　ＷＥＢＥＤＩ　　*
*    モジュール名　　　　：　納品予定データ作成                *
*    作成日／更新日　　　：　2010/06/18                        *
*    作成者／更新者　　　：　NAV ABE                           *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7312B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/06/18.
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
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*ダイユーエイト　発注情報保存データ
     SELECT   DYJOHOF   ASSIGN    TO        DA-01-VI-DYJOHOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DYJ-F01   DYJ-F02
                                            DYJ-F03   DYJ-F04
                                            DYJ-F08   DYJ-F05
                                            DYJ-F06   DYJ-F07
                        FILE      STATUS    DYJ-STATUS.
*ダイユーエイト　出荷情報修正
     SELECT   DYSYUKL1  ASSIGN    TO        DA-01-VI-DYSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DYS-F01   DYS-F02
                                            DYS-F03   DYS-F04
                                            DYS-F05   DYS-F06
                                            DYS-F07   DYS-F08
                        FILE      STATUS    DYS-STATUS.
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
*    ダイユーエイト　　　　　発注情報保存データ
******************************************************************
 FD  DYJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DYJOHOF   OF        XFDLIB
              JOINING   DYJ       PREFIX.
******************************************************************
*    ダイユーエイト　　　　　発注データ
******************************************************************
 FD  DYSYUKL1           LABEL RECORD   IS   STANDARD.
     COPY     DYSYUKL1  OF        XFDLIB
              JOINING   DYS       PREFIX.
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
     03  DYSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
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
     03  DYJ-STATUS        PIC  X(02).
     03  DYS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7312B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7312B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7312B".
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
     03  NOU-F01           PIC  9(07)V9(01)  VALUE  ZERO.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
     03  NOU-F02           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(11)   VALUE  SPACE.
*原価計算要
 01  WK-NOUSU              PIC  9(08)   VALUE  ZERO.
 01  WK-TANKA
     03  WK-GENTAN         PIC  9(09)V9(02)  VALUE  ZERO.
     03  WK-BAITAN         PIC  9(09)V9(02)  VALUE  ZERO.
*納品予定原価金額
 01  WK-GENKA.
     03  GEN-F01           PIC  9(10)   VALUE  ZERO.
*納品予定原価金額
 01  WK-BAIKA              PIC  9(10)   VALUE  ZERO.
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
                        PROCEDURE   DYJOHOF.
     MOVE      "DYJOHOL1 "   TO   AB-FILE.
     MOVE      DYJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYSYUKL1.
     MOVE      "DYSYUKL1 "   TO   AB-FILE.
     MOVE      DYS-STATUS   TO   AB-STS.
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
     OPEN     I-O       DYJOHOF  SHTDENF.
     OPEN     I-O       DYSYUKL1.
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
*    ダイユーエイト　　　　　発注情報保存データスタート
     MOVE     SPACE          TO   DYJ-REC.
     INITIALIZE                   DYJ-REC.
     MOVE     PARA-JDATE     TO   DYJ-F01.
     MOVE     PARA-JTIME     TO   DYJ-F02.
     MOVE     PARA-TORICD    TO   DYJ-F03.
     MOVE     PARA-SOKO      TO   DYJ-F04.
     MOVE     PARA-NOUDT     TO   DYJ-F08.
     MOVE     ZERO           TO   DYJ-F05.
     MOVE     ZERO           TO   DYJ-F06.
     MOVE     ZERO           TO   DYJ-F07.
*    バッチ番号のチェック
     START    DYJOHOF   KEY  >=   DYJ-F01   DYJ-F02
                                  DYJ-F03   DYJ-F04
                                  DYJ-F08   DYJ-F05
                                  DYJ-F06   DYJ-F07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ダイユーエイト　　　　　発注情報保存データ読込み
     PERFORM DYJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　基本情報ファイルＲＥＡＤ　　　　　　　　　　　　　　　　　*
****************************************************************
 DYJOHOF-READ-SEC    SECTION.
*
     READ     DYJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DYJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DYJ-F01
     AND      PARA-JTIME  =  DYJ-F02
     AND      PARA-TORICD =  DYJ-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  DYJOHOF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  DYJ-F03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  DYJOHOF-READ-EXIT
              END-IF
     END-IF.
*    送信ＦＬＧのチェック
     IF       DYJ-F11  =  "1"
              GO                 TO   DYJOHOF-READ-SEC
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  DYJ-F04
                   CONTINUE
              ELSE
                   GO            TO  DYJOHOF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  DYJ-F08
                  CONTINUE
              ELSE
                  GO        TO   DYJOHOF-READ-SEC
              END-IF
     END-IF.
*
 DYJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     DYJ-F03             TO   DEN-F01.
     MOVE     DYJ-F06             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     DYJ-F07             TO   DEN-F03.
     MOVE     DYJ-F05             TO   DEN-F07.
     MOVE     DYJ-F08             TO   DEN-F112.
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
              PERFORM   DYSYUKF-READ-SEC
              IF    DYSYUKF-INV-FLG = "INV"
                    PERFORM   DYSYUKF-WRITE-SEC
                    MOVE   "1"       TO   DYJ-F09
                    MOVE  SYS-DATEW  TO   DYJ-F10
                    REWRITE  DYJ-REC
              END-IF
     END-IF.
 MAIN010.
*    ダイユーエイト 発注情報保存データ読込み
     PERFORM DYJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ダイユー  発注確定データ作成処理
****************************************************************
 DYSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "DYSYUKF-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  DYS-REC.
     INITIALIZE                        DYS-REC.
     MOVE      DYJ-REC             TO  DYS-REC.
*    納品予定年月日
     MOVE      SYS-DATEW           TO  DYS-H08A.
*    ＥＯＳ入力区分
     MOVE      1                   TO  DYS-H10A.
*    システム伝票番号
     MOVE      DYJ-M14             TO  DYS-M14.
*    原単価
     MOVE      DYJ-M12             TO  DYS-M12.
*    売単価
     MOVE      DYJ-M13             TO  DYS-M13.
*    納品数量
     MOVE     SPACE                TO  WK-NOUHIN.
     INITIALIZE                        WK-NOUHIN.
*    COMPUTE   NOU-F01 =  DEN-F15 * 100.
*****MOVE      DEN-F15             TO  DYS-M09A.
     COMPUTE   WK-NOUSU  =   DEN-F15  *  10
     MOVE      WK-NOUSU            TO  DYS-M09A.
*    納品原価金額
     MOVE     ZERO                 TO  WK-GENTAN.
     MOVE     SPACE                TO  WK-GENKA.
     INITIALIZE                        WK-GENKA.
     MOVE      DYS-M12             TO  WK-GENTAN
*****COMPUTE   WK-GENTAN =  DYS-M12   / 100.
     COMPUTE   GEN-F01   =  DEN-F15   * WK-GENTAN.
     MOVE      GEN-F01             TO  DYS-M10.
*    納品売価金額
     MOVE      ZERO                TO  WK-BAIKA  WK-BAITAN.
**** COMPUTE   WK-BAITAN =  DYJ-M13   / 100.
     MOVE      DYJ-M13             TO   WK-BAITAN
     COMPUTE   WK-BAIKA  =  DEN-F15   * WK-BAITAN.
     MOVE      WK-BAIKA            TO  DYS-M11.
*
     MOVE      SPACE               TO  DYS-A24  DYS-A25
                                       DYS-A26  DYS-A27.
*    欠品区分
*****IF        DYS-M09A  NOT =  DYS-M07
     IF        DEN-F15   NOT =  DEN-F50
***************MOVE   "01"         TO  DYS-M14A
               EVALUATE DEN-F411
                   WHEN "1"   MOVE  "01"   TO  DYS-M14A
                   WHEN "2"   MOVE  "02"   TO  DYS-M14A
                   WHEN "3"   MOVE  "03"   TO  DYS-M14A
                   WHEN "4"   MOVE  "04"   TO  DYS-M14A
                   WHEN OTHER MOVE  "01"   TO  DYS-M14A
              END-EVALUATE
     ELSE
               MOVE   "00"         TO  DYS-M14A
     END-IF.
*
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
     DISPLAY "ﾀﾞｲﾕｰｴｲﾄ  F     READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ           CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "ﾀﾞｲﾕｰｴｲﾄ ｶｸﾃｲDT WT   CNT = " KOS1-CNT  UPON CONS.
*
     CLOSE     SHTDENF  DYJOHOF  DYSYUKL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　出荷情報ファイルＲＥＡＤ                                  *
****************************************************************
 DYSYUKF-READ-SEC    SECTION.
*
     MOVE     SPACE          TO   DYS-REC.
     INITIALIZE                   DYS-REC.
     MOVE     DYJ-F01        TO   DYS-F01.
     MOVE     DYJ-F02        TO   DYS-F02.
     MOVE     DYJ-F03        TO   DYS-F03.
     MOVE     DYJ-F04        TO   DYS-F04.
     MOVE     DYJ-F08        TO   DYS-F08.
     MOVE     DYJ-F05        TO   DYS-F05.
     MOVE     DYJ-F06        TO   DYS-F06.
     MOVE     DYJ-F07        TO   DYS-F07.
     MOVE     DYJ-F08        TO   DYS-F08.
     READ     DYSYUKL1   INVALID
              MOVE    "INV"       TO   DYSYUKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   DYSYUKF-INV-FLG
     END-READ.
*
 DYSYUKF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
