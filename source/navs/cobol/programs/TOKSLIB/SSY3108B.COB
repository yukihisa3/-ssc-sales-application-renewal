****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　カーマオンラインシステム　　　　　*
*    業務名　　　　　　　：　カーマオンラインシステム　　　　　*
*    モジュール名　　　　：　出荷送信データ作成１              *
*    作成日／更新日　　　：　2002/04/05                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　カーマへの送信データを作成する。  *
*                            送信データ作成１                  *
*    2004/01/13 NAV 出荷数＝ゼロは送信ＤＴ作成しない。         *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3108B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/04/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*カーマ出荷確定データ
     SELECT   KMSYUKF   ASSIGN    TO        DA-01-VI-KMSYUKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KMK-F13   KMK-F09A
                                            KMK-F05   KMK-F06
                                            KMK-F07
                        FILE      STATUS    KMK-STATUS.
*カーマ出荷情報保存データ
     SELECT   KMJOHOF   ASSIGN    TO        DA-01-VI-KMJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KMS-F01   KMS-F02
                                            KMS-F03   KMS-F04
                                            KMS-F05   KMS-F06
                                            KMS-F07
                        FILE      STATUS    KMS-STATUS.
*カーマ送信情報データ
     SELECT   KMHENKF   ASSIGN    TO        DA-01-VI-KMHENKL1
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
*    カーマ出荷確定データ
******************************************************************
 FD  KMSYUKF
                        LABEL RECORD   IS   STANDARD.
     COPY     KMSYUKF   OF        XFDLIB
              JOINING   KMK  AS   PREFIX.
*
******************************************************************
*    カーマ出荷情報保存データ
******************************************************************
 FD  KMJOHOF
                        LABEL RECORD   IS   STANDARD.
     COPY     KMJOHOF   OF        XFDLIB
              JOINING   KMS  AS   PREFIX.
*
******************************************************************
*    カーマ送信情報データ
******************************************************************
 FD  KMHENKF            LABEL RECORD   IS   STANDARD.
     COPY     KMHENKF   OF        XFDLIB
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
     03  KMSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  KMJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
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
         05  ST-PG          PIC   X(08)  VALUE "SSY3108B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3108B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3108B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KMSYUKF.
     MOVE      "KMSYUKL3"   TO   AB-FILE.
     MOVE      KMK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KMJOHOF.
     MOVE      "KMJOHOL1"   TO   AB-FILE.
     MOVE      KMS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KMHENKF.
     MOVE      "KMHENKL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
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
     OPEN     I-O       KMSYUKF  KMJOHOF.
     OPEN     OUTPUT    KMHENKF.
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
*    カーマ出荷確定データ読込み
     PERFORM KMSYUKF-READ-SEC.
*    終了条件判定
     IF       END-FLG  NOT =  "END"
              MOVE     KMK-F09A   TO   WK-KMK-F09A
              MOVE     KMK-F05    TO   WK-KMK-F05
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
     IF       KMK-F15  <=  ZERO
              GO                  TO   MAIN010
     END-IF.
*    発注日／店舗ＣＤがブレイクした場合
     IF       KMK-F09A  NOT =  WK-KMK-F09A
     OR       KMK-F05   NOT =  WK-KMK-F05
              PERFORM  KMHENKF-WT2-SEC
              MOVE     ZERO       TO   WK-DEN-CNT WK-REC-CNT
              MOVE     KMK-F09A   TO   WK-KMK-F09A
              MOVE     KMK-F05    TO   WK-KMK-F05
     END-IF.
*    伝票枚数カウント
     IF       KMK-F06  NOT =  WK-KMK-F06
              ADD       1         TO   WK-DEN-CNT
              MOVE   KMK-F06      TO   WK-KMK-F06
     END-IF.
*    カーマ送信情報データ（レコード区分＝２）作成
     ADD      1                   TO   WK-REC-CNT.
     PERFORM  KMHENKF-WT1-SEC.
 MAIN010.
*    カーマ出荷確定データ更新
     MOVE     "1"                 TO   KMK-F13.
     MOVE     LINK-OUT-YMD8       TO   KMK-F14.
     REWRITE  KMK-REC.
*    カーマ出荷情報保存データ更新
     PERFORM  KMJOHOF-READ-SEC.
     IF       KMJOHOF-INV-FLG =  SPACE
              MOVE    "1"     TO    KMS-F13
              REWRITE  KMS-REC
     END-IF.
*    カーマ出荷確定データ読込み
     PERFORM  KMSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　カーマ出荷確定データ読込み　　　　　　　　　　　　　　　　*
****************************************************************
 KMSYUKF-READ-SEC    SECTION.
*
     READ     KMSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KMSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    送信ＦＬＧチェック
     IF       KMK-F13  =  1
              MOVE     "END"    TO  END-FLG
     END-IF.
*
 KMSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　カーマ出荷情報保存データ読込み                            *
****************************************************************
 KMJOHOF-READ-SEC    SECTION.
*
     MOVE     KMK-F01       TO      KMS-F01.
     MOVE     KMK-F02       TO      KMS-F02.
     MOVE     KMK-F03       TO      KMS-F03.
     MOVE     KMK-F04       TO      KMS-F04.
     MOVE     KMK-F05       TO      KMS-F05.
     MOVE     KMK-F06       TO      KMS-F06.
*****MOVE     KMK-F07       TO      KMS-F07.
     MOVE     KMK-F105      TO      KMS-F07.
     READ     KMJOHOF
              INVALID
              MOVE   "INV"  TO      KMJOHOF-INV-FLG
              NOT INVALID
              MOVE   SPACE  TO      KMJOHOF-INV-FLG
     END-READ.
*
 KMJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　カーマ送信情報データ作成１                                *
****************************************************************
 KMHENKF-WT1-SEC       SECTION.
*
     MOVE     "KMHENKF-WT1-SEC"    TO  S-NAME.
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
     WRITE     HEN-REC.
     ADD       1                   TO  HEN1-CNT.
*
 KMHENKF-WT1-EXIT.
     EXIT.
****************************************************************
*　　カーマ送信情報データ作成２                                *
****************************************************************
 KMHENKF-WT2-SEC       SECTION.
*
     MOVE     "KMHENKF-WT2-SEC"    TO  S-NAME.
*
     MOVE      SPACE               TO  HEN-REC.
     INITIALIZE                        HEN-REC.
     MOVE      WK-KMK-F09A         TO  HEN-F01.
     MOVE      WK-KMK-F05          TO  HEN-F02.
     MOVE      "1"                 TO  HEN-F03.
     MOVE      WK-REC-CNT          TO  HEN-F06.
     MOVE      WK-DEN-CNT          TO  HEN-F07.
     WRITE     HEN-REC.
     ADD       1                   TO  HEN2-CNT.
*
 KMHENKF-WT2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     PERFORM  KMHENKF-WT2-SEC.
*
     DISPLAY "ｶｰﾏ ｼｭｯｶｶｸﾃｲ RD CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｶｰﾏ ｿｳｼﾝ1       CNT = " HEN1-CNT  UPON CONS.
     DISPLAY "ｶｰﾏ ｿｳｼﾝ2       CNT = " HEN2-CNT  UPON CONS.
*
     CLOSE     KMSYUKF  KMJOHOF  KMHENKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
