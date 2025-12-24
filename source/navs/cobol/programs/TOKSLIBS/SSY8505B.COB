****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　くろがねやオンラインシステム　　　*
*    業務名　　　　　　　：　くろがねやオンラインシステム　　　*
*    モジュール名　　　　：　出荷送信データ作成１              *
*    作成日／更新日　　　：　2006/12/11                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　くろがねや送信ＤＴを作成する。    *
*                            送信データ作成１                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8505B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/12/11.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*くろがねや出荷確定データ
     SELECT   KGSYUKF   ASSIGN    TO        DA-01-VI-KGSYUKL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KMK-F13   KMK-F01
                                            KMK-F02   KMK-F03
                                            KMK-F04   KMK-F09A
                                            KMK-F05   KMK-F06
                                            KMK-F07
                        FILE      STATUS    KMK-STATUS.
*くろがねや出荷情報保存データ
     SELECT   KGJOHOF   ASSIGN    TO        DA-01-VI-KGJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KMS-F01   KMS-F02
                                            KMS-F03   KMS-F04
                                            KMS-F05   KMS-F06
                                            KMS-F07
                        FILE      STATUS    KMS-STATUS.
*くろがねや送信情報データ
     SELECT   KGHENKF   ASSIGN    TO        DA-01-VI-KGHENKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEN-F01   HEN-F02
                                            HEN-F03   HEN-F04
                                            HEN-F05
                        FILE      STATUS    HEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    くろがねや出荷確定データ
******************************************************************
 FD  KGSYUKF
                        LABEL RECORD   IS   STANDARD.
     COPY     KGSYUKF   OF        XFDLIB
              JOINING   KMK  AS   PREFIX.
*
******************************************************************
*    くろがねや出荷情報保存データ
******************************************************************
 FD  KGJOHOF
                        LABEL RECORD   IS   STANDARD.
     COPY     KGJOHOF   OF        XFDLIB
              JOINING   KMS  AS   PREFIX.
*
******************************************************************
*    くろがねや送信情報データ
******************************************************************
 FD  KGHENKF            LABEL RECORD   IS   STANDARD.
     COPY     KGHENKF   OF        XFDLIB
              JOINING   HEN       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  WK-DEN-CNT          PIC  9(08)     VALUE  ZERO.
     03  WK-REC-CNT          PIC  9(08)     VALUE  ZERO.
     03  HEN1-CNT            PIC  9(08)     VALUE  ZERO.
     03  HEN2-CNT            PIC  9(08)     VALUE  ZERO.
 01  WK-KEY-JYOHO.
     03  WK-KMK-F09A         PIC  9(08)     VALUE  ZERO.
     03  WK-KMK-F05          PIC  9(05)     VALUE  ZERO.
     03  WK-KMK-F06          PIC  9(09)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KGSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  KGJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  KMK-STATUS        PIC  X(02).
     03  KMS-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8505B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8505B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8505B".
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
 01  LINK-HIDUKE        PIC  9(08).
 01  LINK-JIKAN         PIC  9(04).
 01  LINK-TOKCD         PIC  9(08).
 01  LINK-SOKCD         PIC  X(02).
 01  LINK-NOUDT         PIC  9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE    DIVISION USING LINK-HIDUKE LINK-JIKAN LINK-TOKCD
                             LINK-SOKCD  LINK-NOUDT.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KGSYUKF.
     MOVE      "KGSYUKL3"   TO   AB-FILE.
     MOVE      KMK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KGJOHOF.
     MOVE      "KGJOHOL1"   TO   AB-FILE.
     MOVE      KMS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KGHENKF.
     MOVE      "KGHENKL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY  "HEN-F01 = "  HEN-F01  UPON CONS.
     DISPLAY  "HEN-F02 = "  HEN-F02  UPON CONS.
     DISPLAY  "HEN-F03 = "  HEN-F03  UPON CONS.
     DISPLAY  "HEN-F04 = "  HEN-F04  UPON CONS.
     DISPLAY  "HEN-F05 = "  HEN-F05  UPON CONS.
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
     OPEN     I-O       KGSYUKF  KGJOHOF.
     OPEN     OUTPUT    KGHENKF.
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
*    くろがねや出荷確定データスタート
     MOVE    SPACE               TO        KMK-REC.
     INITIALIZE                            KMK-REC.
     MOVE    SPACE               TO        KMK-F13.
     MOVE    LINK-HIDUKE         TO        KMK-F01.
     MOVE    LINK-JIKAN          TO        KMK-F02.
     MOVE    LINK-TOKCD          TO        KMK-F03.
     MOVE    LINK-SOKCD          TO        KMK-F04.
     START  KGSYUKF KEY IS >= KMK-F13  KMK-F01  KMK-F02  KMK-F03
                              KMK-F04  KMK-F09A KMK-F05  KMK-F06
                              KMK-F07
            INVALID
            MOVE    "END"        TO        END-FLG
            DISPLAY "＃対象データ無し１＃" UPON CONS
            GO                   TO        INIT-EXIT
     END-START.
*    くろがねや出荷確定データ読込み
     PERFORM KGSYUKF-READ-SEC.
*    終了条件判定
     IF       END-FLG  NOT =  "END"
              MOVE     KMK-F09A   TO   WK-KMK-F09A
              MOVE     KMK-F05    TO   WK-KMK-F05
     ELSE
              MOVE    "END"       TO   END-FLG
              DISPLAY "＃対象データ無し２＃" UPON CONS
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
*実出荷数＝０の場合は処理対象外とする。
*****IF       KMK-F15  <=  ZERO
*             くろがねや出荷確定データ更新
**            MOVE     "1"                 TO   KMK-F13.
**            MOVE     LINK-OUT-YMD8       TO   KMK-F14.
**            REWRITE  KMK-REC.
*             くろがねや出荷情報保存データ更新
**            PERFORM  KGJOHOF-READ-SEC.
**            IF       KGJOHOF-INV-FLG =  SPACE
**                     MOVE    "1"     TO    KMS-F13
**                     REWRITE  KMS-REC
**            END-IF.
**************GO                  TO   MAIN010
*****END-IF.
*    発注日／店舗ＣＤがブレイクした場合
     IF       KMK-F09A  NOT =  WK-KMK-F09A
     OR       KMK-F05   NOT =  WK-KMK-F05
              PERFORM  KGHENKF-WT2-SEC
              MOVE     ZERO       TO   WK-DEN-CNT WK-REC-CNT
              MOVE     KMK-F09A   TO   WK-KMK-F09A
              MOVE     KMK-F05    TO   WK-KMK-F05
     END-IF.
*    伝票枚数カウント
     IF       KMK-F06  NOT =  WK-KMK-F06
              ADD       1         TO   WK-DEN-CNT
              MOVE   KMK-F06      TO   WK-KMK-F06
     END-IF.
*    くろがねや送信情報データ（レコード区分＝２）作成
     ADD      1                   TO   WK-REC-CNT.
*    くろがねや出荷確定データ更新
     MOVE     "1"                 TO   KMK-F13.
     MOVE     LINK-OUT-YMD8       TO   KMK-F14.
     REWRITE  KMK-REC.
*    くろがねや出荷情報保存データ更新
     PERFORM  KGJOHOF-READ-SEC.
     IF       KGJOHOF-INV-FLG =  SPACE
              MOVE    "1"     TO    KMS-F13
              REWRITE  KMS-REC
     END-IF.
     PERFORM  KGHENKF-WT1-SEC.
 MAIN010.
*    くろがねや出荷確定データ読込み
     PERFORM  KGSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　カーマ出荷確定データ読込み　　　　　　　　　　　　　　　　*
****************************************************************
 KGSYUKF-READ-SEC    SECTION.
*
     READ     KGSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KGSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    送信ＦＬＧチェック
     IF       KMK-F13  =  1
              MOVE     "END"    TO  END-FLG
              GO                TO  KGSYUKF-READ-EXIT
     END-IF.
*    バッチ_チェック
     IF       KMK-F01  =  LINK-HIDUKE
     AND      KMK-F02  =  LINK-JIKAN
     AND      KMK-F03  =  LINK-TOKCD
              CONTINUE
     ELSE
              MOVE     "END"    TO  END-FLG
              GO                TO  KGSYUKF-READ-EXIT
     END-IF.
*    倉庫チェック
     IF       LINK-SOKCD  =  SPACE
              CONTINUE
     ELSE
              IF   KMK-F04  =  LINK-SOKCD
                   CONTINUE
              ELSE
                   GO           TO  KGSYUKF-READ-SEC
              END-IF
     END-IF.
*    納品日チェック
     IF       LINK-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF   KMK-F09B  =  LINK-NOUDT
                   CONTINUE
              ELSE
                   GO           TO  KGSYUKF-READ-SEC
              END-IF
     END-IF.
*
 KGSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　カーマ出荷情報保存データ読込み                            *
****************************************************************
 KGJOHOF-READ-SEC    SECTION.
*
     MOVE     KMK-F01       TO      KMS-F01.
     MOVE     KMK-F02       TO      KMS-F02.
     MOVE     KMK-F03       TO      KMS-F03.
     MOVE     KMK-F04       TO      KMS-F04.
     MOVE     KMK-F05       TO      KMS-F05.
     MOVE     KMK-F06       TO      KMS-F06.
*****MOVE     KMK-F07       TO      KMS-F07.
     MOVE     KMK-F105      TO      KMS-F07.
     READ     KGJOHOF
              INVALID
              MOVE   "INV"  TO      KGJOHOF-INV-FLG
              NOT INVALID
              MOVE   SPACE  TO      KGJOHOF-INV-FLG
     END-READ.
*
 KGJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　カーマ送信情報データ作成１                                *
****************************************************************
 KGHENKF-WT1-SEC       SECTION.
*
     MOVE     "KGHENKF-WT1-SEC"    TO  S-NAME.
*
     MOVE      SPACE               TO  HEN-REC.
     INITIALIZE                        HEN-REC.
     MOVE      KMK-F09A            TO  HEN-F01.
     MOVE      KMK-F05             TO  HEN-F02.
     MOVE      "2"                 TO  HEN-F03.
     MOVE      KMK-F06             TO  HEN-F04.
     MOVE      KMK-F07             TO  HEN-F05.
     MOVE      KMK-F09             TO  HEN-F09.
     MOVE      KMK-F10             TO  HEN-F10.
     MOVE      KMK-F15             TO  HEN-F11.
*****データ種別セット
     MOVE      KMS-F092            TO  HEN-F092.
     WRITE     HEN-REC.
     ADD       1                   TO  HEN1-CNT.
*
 KGHENKF-WT1-EXIT.
     EXIT.
****************************************************************
*　　カーマ送信情報データ作成２                                *
****************************************************************
 KGHENKF-WT2-SEC       SECTION.
*
     MOVE     "KGHENKF-WT2-SEC"    TO  S-NAME.
*
     MOVE      SPACE               TO  HEN-REC.
     INITIALIZE                        HEN-REC.
     MOVE      WK-KMK-F09A         TO  HEN-F01.
     MOVE      WK-KMK-F05          TO  HEN-F02.
     MOVE      "1"                 TO  HEN-F03.
     MOVE      WK-REC-CNT          TO  HEN-F06.
     MOVE      WK-DEN-CNT          TO  HEN-F07.
*****データ種別セット
     MOVE      KMS-F092            TO  HEN-F092.
     WRITE     HEN-REC.
     ADD       1                   TO  HEN2-CNT.
*
 KGHENKF-WT2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     PERFORM  KGHENKF-WT2-SEC.
*
     DISPLAY "ｶｰﾏ ｼｭｯｶｶｸﾃｲ RD CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｶｰﾏ ｿｳｼﾝ1       CNT = " HEN1-CNT  UPON CONS.
     DISPLAY "ｶｰﾏ ｿｳｼﾝ2       CNT = " HEN2-CNT  UPON CONS.
*
     CLOSE     KGSYUKF  KGJOHOF  KGHENKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
