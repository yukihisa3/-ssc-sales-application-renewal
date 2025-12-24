****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　カーマオンラインシステム　　　　　*
*    業務名　　　　　　　：　カーマオンラインシステム　　　　　*
*    モジュール名　　　　：　出荷確定データ作成                *
*    作成日／更新日　　　：　2002/04/04                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3104B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/04/04.
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
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*カーマ出荷情報保存データ
     SELECT   KMJOHOF   ASSIGN    TO        DA-01-VI-KMJOHOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KMS-F01   KMS-F02
                                            KMS-F03   KMS-F04
                                            KMS-F09B  KMS-F05
                                            KMS-F06   KMS-F07
                        FILE      STATUS    KMS-STATUS.
*カーマ出荷確定データ
     SELECT   KMSYUKF   ASSIGN    TO        DA-01-VI-KMSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KMK-F01   KMK-F02
                                            KMK-F03   KMK-F04
                                            KMK-F05   KMK-F06
                                            KMK-F07
                        FILE      STATUS    KMK-STATUS.
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
*    カーマ出荷情報保存データ
******************************************************************
 FD  KMJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     KMJOHOF   OF        XFDLIB
              JOINING   KMS       PREFIX.
******************************************************************
*    カーマ出荷確定データ
******************************************************************
 FD  KMSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     KMSYUKF   OF        XFDLIB
              JOINING   KMK       PREFIX.
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
     03  KMK1-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KMSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
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
     03  KMS-STATUS        PIC  X(02).
     03  KMK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3104B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3104B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3104B".
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
                        PROCEDURE   KMJOHOF.
     MOVE      "KMJOHOF "   TO   AB-FILE.
     MOVE      KMS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KMSYUKF.
     MOVE      "KMSYUKF "   TO   AB-FILE.
     MOVE      KMK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     DISPLAY "KMK-F01 = " KMK-F01 UPON CONS.
     DISPLAY "KMK-F02 = " KMK-F02 UPON CONS.
     DISPLAY "KMK-F03 = " KMK-F03 UPON CONS.
     DISPLAY "KMK-F04 = " KMK-F04 UPON CONS.
     DISPLAY "KMK-F05 = " KMK-F05 UPON CONS.
     DISPLAY "KMK-F06 = " KMK-F06 UPON CONS.
     DISPLAY "KMK-F07 = " KMK-F07 UPON CONS.
     DISPLAY "KMS-F13 = " KMS-F13 UPON CONS.
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
     OPEN     INPUT     SHTDENF.
     OPEN     I-O       KMJOHOF  KMSYUKF.
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
*    カーマ出荷情報保存データスタート
     MOVE     SPACE          TO   KMS-REC.
     INITIALIZE                   KMS-REC.
     MOVE     PARA-JDATE     TO   KMS-F01.
     MOVE     PARA-JTIME     TO   KMS-F02.
     MOVE     PARA-TORICD    TO   KMS-F03.
     MOVE     PARA-SOKO      TO   KMS-F04.
     MOVE     PARA-NOUDT     TO   KMS-F09B.
     MOVE     ZERO           TO   KMS-F05.
     MOVE     ZERO           TO   KMS-F06.
     MOVE     ZERO           TO   KMS-F07.
     START    KMJOHOF   KEY  >=   KMS-F01   KMS-F02
                                  KMS-F03   KMS-F04
                                  KMS-F09B  KMS-F05
                                  KMS-F06   KMS-F07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    カーマ出荷情報保存データ読込み
     PERFORM KMJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KMJOHOF-READ-SEC    SECTION.
*
     READ     KMJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KMJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  KMS-F01
     AND      PARA-JTIME  =  KMS-F02
     AND      PARA-TORICD =  KMS-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  KMJOHOF-READ-EXIT
     END-IF.
*    送信ＦＬＧのチェック
     IF       KMS-F13  =  "1"
              GO                 TO   KMJOHOF-READ-SEC
     END-IF.
     IF       PARA-SOKO   =  SPACE
              GO                 TO   KMJOHOF-READ-EXIT
     END-IF.
*    抽出条件のチェック
     IF       PARA-SOKO   =  KMS-F04
              IF   PARA-NOUDT  =  ZERO
                   CONTINUE
              ELSE
                   IF  PARA-NOUDT  =  KMS-F09B
                       CONTINUE
                   ELSE
                       GO        TO   KMJOHOF-READ-SEC
                   END-IF
              END-IF
     ELSE
              GO                 TO   KMJOHOF-READ-SEC
     END-IF.
*
 KMJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    カーマ出荷確定データ検索
*    存在した場合＝送信ＦＬＧがゼロの場合は出荷数のみ更新
*    未存在の場合＝カーマ出荷情報保存データを確定データへ更新
*    MOVE     KMS-F01             TO   KMK-F01.
*    MOVE     KMS-F02             TO   KMK-F02.
*    MOVE     KMS-F03             TO   KMK-F03.
*    MOVE     KMS-F04             TO   KMK-F04.
*    MOVE     KMS-F05             TO   KMK-F05.
*    MOVE     KMS-F06             TO   KMK-F06.
*    MOVE     KMS-F07             TO   KMK-F07.
*    READ     KMSYUKF    INVALID
*             MOVE    "INV"       TO   KMSYUKF-INV-FLG
*             NOT  INVALID
*             MOVE    SPACE       TO   KMSYUKF-INV-FLG
*    END-READ.
*    伝票番号ブレイクチェック
     IF       KMS-F06  NOT =  WK-KMS-F06
              MOVE    ZERO        TO   WK-GYO-CNT
              MOVE    KMS-F06     TO   WK-KMS-F06
     END-IF.
*    カーマ出荷確定データ削除
*    IF       KMSYUKF-INV-FLG = SPACE
*             DELETE  KMSYUKF
*    END-IF.
*    売上伝票ファイル検索
     MOVE     KMS-F03             TO   DEN-F01.
     MOVE     KMS-F06             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     KMS-F07             TO   DEN-F03.
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
**            IF   KMSYUKF-INV-FLG  =  SPACE
**                 PERFORM   KMSYUKF-REWRITE-SEC
**            ELSE
**                 PERFORM   KMSYUKF-WRITE-SEC
**            END-IF
**************数量確認
              IF   DEN-F15  NOT =  ZERO
                   PERFORM   KMSYUKF-WRITE-SEC
              ELSE
*******************数量＝０の時は、送信済とする。
                   MOVE      "1"                 TO  KMS-F11
                   MOVE      LINK-OUT-YMD8       TO  KMS-F12
                   MOVE      "1"                 TO  KMS-F13
                   REWRITE   KMS-REC
                   ADD       1                   TO  KMS-CNT
              END-IF
     END-IF.
 MAIN010.
*    カーマ出荷情報保存データ読込み
     PERFORM KMJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　カーマ出荷確定データ作成処理　　　　　　　　　　　　　　　*
****************************************************************
 KMSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "KMSYUKF-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  KMK-REC.
     INITIALIZE                        KMK-REC.
     MOVE      KMS-F01             TO  KMK-F01.
     MOVE      KMS-F02             TO  KMK-F02.
     MOVE      KMS-F03             TO  KMK-F03.
     MOVE      KMS-F04             TO  KMK-F04.
     MOVE      KMS-F05             TO  KMK-F05.
     MOVE      KMS-F06             TO  KMK-F06.
*****行番号の変更
*****MOVE      KMS-F07             TO  KMK-F07.
     ADD       1                   TO  WK-GYO-CNT.
     MOVE      WK-GYO-CNT          TO  KMK-F07.
     MOVE      KMS-F09             TO  KMK-F09.
     MOVE      KMS-F10             TO  KMK-F10.
     MOVE      "1"                 TO  KMS-F11.
     MOVE      LINK-OUT-YMD8       TO  KMS-F12.
     MOVE      LINK-OUT-YMD8       TO  KMK-F14.
     COMPUTE   KMK-F15  =  DEN-F15  *  10.
**   DISPLAY "KMK-F01 = " KMK-F01 UPON CONS.
*    DISPLAY "KMK-F02 = " KMK-F02 UPON CONS.
*    DISPLAY "KMK-F03 = " KMK-F03 UPON CONS.
*    DISPLAY "KMK-F04 = " KMK-F04 UPON CONS.
*    DISPLAY "KMK-F05 = " KMK-F05 UPON CONS.
*    DISPLAY "KMK-F06 = " KMK-F06 UPON CONS.
**   DISPLAY "KMK-F07 = " KMK-F07 UPON CONS.
     WRITE     KMK-REC.
     ADD       1                   TO  KMK1-CNT.
     REWRITE   KMS-REC.
     ADD       1                   TO  KMS-CNT.
*
 KMSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　カーマ出荷確定データ更新処理　　　　　　　　　　　　　　　*
****************************************************************
 KMSYUKF-REWRITE-SEC     SECTION.
*
     MOVE   "KMSYUKF-REWRITE-SEC"  TO  S-NAME.
*
     MOVE      "1"                 TO  KMS-F11.
     MOVE      LINK-OUT-YMD8       TO  KMS-F12.
     MOVE      LINK-OUT-YMD8       TO  KMK-F14.
     COMPUTE   KMK-F15  =  DEN-F15  *  10.
     REWRITE   KMK-REC.
     ADD       1                   TO  KMK2-CNT.
     REWRITE   KMS-REC.
     ADD       1                   TO  KMS-CNT.
*
 KMSYUKF-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ｶｰﾏF       READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｶｰﾏ ｿｳｼﾝｽﾞﾐ     CNT = " SKIP1-CNT UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ      CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "ｶｰﾏ ｶｸﾃｲDT WT   CNT = " KMK1-CNT  UPON CONS.
     DISPLAY "ｶｰﾏF       REWT CNT = " KMS-CNT   UPON CONS.
     DISPLAY "ｶｰﾏ ｶｸﾃｲDT REWT CNT = " KMK2-CNT  UPON CONS.
*
     CLOSE     SHTDENF  KMJOHOF  KMSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
