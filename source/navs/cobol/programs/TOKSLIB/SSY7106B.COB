****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｊ本田オンラインシステム　　　　　*
*    業務名　　　　　　　：　Ｊ本田オンラインシステム　　　　　*
*    モジュール名　　　　：　出荷確定データ作成                *
*    作成日／更新日　　　：　2005/05/24                        *
*    作成者／更新者　　　：　C FUJIWARA                        *
*    処理概要　　　　　　：　受取ったパラメタより対象データを  *
*                            Ｊ本田出荷確定データより抽出する。*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7106B.
 AUTHOR.                C FUJIWARA.
 DATE-WRITTEN.          05/05/24.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    TBL-STATUS.
*Ｊ本田基本情報データ
     SELECT   HDJOHOF   ASSIGN    TO        DA-01-VI-HDJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HDJ-F01   HDJ-F04
                                            HDJ-F06
                        FILE      STATUS    HDJ-STATUS.
*Ｊ本田出荷確定データ
     SELECT   HDSYUKF   ASSIGN    TO        DA-01-VI-HDSYUKL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HDK-F01   HDK-F03
                                            HDK-F04   HDK-F06
                        FILE      STATUS    HDK-STATUS.
*入荷予定ワーク
     SELECT   HDHEADF   ASSIGN    TO        DA-01-VI-HDHEADL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HNY-F01   HNY-F02
                                            HNY-F03   HNY-F04
                                            HNY-F05   HNY-F06
                        FILE      STATUS    HDK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL
                        LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL  AS   PREFIX.
*
******************************************************************
*    Ｊ本田基本情報データ
******************************************************************
 FD  HDJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     HDJOHOF   OF        XFDLIB
              JOINING   HDJ       PREFIX.
******************************************************************
*    Ｊ本田出荷確定データ
******************************************************************
 FD  HDSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     HDSYUKF   OF        XFDLIB
              JOINING   HDK       PREFIX.
*
******************************************************************
*    入荷予定ワーク
******************************************************************
 FD  HDHEADF            LABEL RECORD   IS   STANDARD.
     COPY     HDHEADF   OF        XFDLIB
              JOINING   HNY       PREFIX.
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
     03  HDK-CNT             PIC  9(08)     VALUE  ZERO.
     03  HNY-CNT             PIC  9(08)     VALUE  ZERO.
     03  HDJ-CNT             PIC  9(08)     VALUE  ZERO.
     03  SEQ-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
*    ﾌﾗｸﾞ
 01  WK-INV-FLG.
     03  HDSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)     VALUE  SPACE.
*    ﾊﾞｯｸｱｯﾌﾟ
 01  WK-HDK-F01              PIC  9(08).
 01  WK-HDK-F02              PIC  9(04).
 01  WK-HDK-F03              PIC  9(08).
 01  WK-HDK-F04              PIC  9(05).
 01  WK-HDK-F05              PIC  9(09).
 01  WK-HDK-A05              PIC  X(05).
 01  WK-HDK-A06              PIC  X(01).
 01  WK-HNY-M1-A12           PIC S9(08)V9(02)  VALUE  ZERO.
*集計
 01  WK-GOKEI.
     03  WK-GK-SURYO         PIC S9(06)V9(02).
     03  WK-GK-GENKA         PIC S9(09)V9(02).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE             PIC  9(06).
     03  SYS-DATEW            PIC  9(08).
*システム時間の編集
     03  SYS-TIME             PIC  9(08).
     03  FILLER            REDEFINES      SYS-TIME.
         05  SYS-TIMEW        PIC  9(06).
         05  SYS-MS           PIC  9(02).
 01  WK-ST.
     03  TBL-STATUS        PIC  X(02).
     03  HDJ-STATUS        PIC  X(02).
     03  HDK-STATUS        PIC  X(02).
     03  HNY-STATUS        PIC  X(02).
*
 01  HNY-F1.
     03  HNY-F1-A01        PIC  X(02).
     03  HNY-F1-A02        PIC  X(02).
     03  HNY-F1-A03        PIC  9(05).
     03  HNY-F1-A04        PIC  9(08).
     03  HNY-F1-A05        PIC  9(06).
     03  HNY-F1-A06        PIC  9(08).
     03  HNY-F1-A07        PIC  9(06).
     03  HNY-F1-A08        PIC  9(05).
     03  HNY-F1-A09        PIC  X(01).
     03  HNY-F1-A10        PIC  X(05).
     03  HNY-F1-A11        PIC  X(01).
     03  HNY-F1-A12        PIC  X(79).
*
 01  HNY-H1.
     03  HNY-H1-A01        PIC  X(02).
     03  HNY-H1-A02        PIC  X(02).
     03  HNY-H1-A03        PIC  9(05).
     03  HNY-H1-A04        PIC  X(05).
     03  HNY-H1-A05        PIC  X(01).
     03  HNY-H1-A06        PIC  X(20).
     03  HNY-H1-A07        PIC  9(08).
     03  HNY-H1-A08        PIC  9(04).
     03  HNY-H1-A09        PIC  X(04).
     03  HNY-H1-A10        PIC  X(04).
     03  HNY-H1-A11        PIC  X(10).
     03  HNY-H1-A12        PIC  X(02).
     03  HNY-H1-A13        PIC  9(08).
     03  HNY-H1-A14        PIC S9(05).
     03  HNY-H1-A15        PIC  X(03).
     03  HNY-H1-A16        PIC  X(02).
     03  HNY-H1-A17        PIC  X(43).
*
 01  HNY-H2.
     03  HNY-H2-A01        PIC  X(02).
     03  HNY-H2-A02        PIC  X(02).
     03  HNY-H2-A03        PIC  9(05).
     03  HNY-H2-A04        PIC  X(04).
     03  HNY-H2-A05        PIC  X(04).
     03  HNY-H2-A06        PIC  X(01).
     03  HNY-H2-A07        PIC  9(07).
     03  HNY-H2-A08        PIC  9(07).
     03  HNY-H2-A09        PIC S9(09)V9(02).
     03  HNY-H2-A10        PIC S9(09)V9(02).
     03  HNY-H2-A11        PIC  X(01).
     03  HNY-H2-A12        PIC  X(20).
     03  HNY-H2-A13        PIC  X(53).
*
 01  HNY-M1.
     03  HNY-M1-A01        PIC  X(02).
     03  HNY-M1-A02        PIC  X(02).
     03  HNY-M1-A03        PIC  9(05).
     03  HNY-M1-A04        PIC  X(02).
     03  HNY-M1-A05        PIC  9(02).
     03  HNY-M1-A06        PIC  9(02).
     03  HNY-M1-A07        PIC  X(01).
     03  HNY-M1-A08        PIC  X(13).
     03  HNY-M1-A09        PIC S9(06)V9(02).
     03  HNY-M1-A10        PIC S9(05).
     03  HNY-M1-A11        PIC S9(06)V9(02).
     03  HNY-M1-A12        PIC S9(08)V9(02).
     03  HNY-M1-A13        PIC S9(08)V9(02).
     03  HNY-M1-A14        PIC S9(06)V9(02).
     03  HNY-M1-A15        PIC  9(08).
     03  HNY-M1-A16        PIC  X(20).
     03  HNY-M1-A17        PIC  X(22).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7106B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7106B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7106B".
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
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HDJOHOF.
     MOVE      "HDJOHOF "   TO   AB-FILE.
     MOVE      HDJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HDSYUKF.
     MOVE      "HDSYUKF "   TO   AB-FILE.
     MOVE      HDK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     DISPLAY "HDK-F01 = " HDK-F01 UPON CONS.
     DISPLAY "HDK-F02 = " HDK-F02 UPON CONS.
     DISPLAY "HDK-F04 = " HDK-F04 UPON CONS.
     DISPLAY "HDK-F06 = " HDK-F06 UPON CONS.
     DISPLAY "HDJ-F092= " HDJ-F092 UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HDHEADF.
     MOVE      "HDHEADF "   TO   AB-FILE.
     MOVE      HNY-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
******************************************************************
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
     OPEN     INPUT     HSHOTBL.
     OPEN     I-O       HDJOHOF  HDSYUKF.
     OPEN     OUTPUT    HDHEADF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT   WK-GOKEI.
     MOVE     1         TO        SEQ-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     ACCEPT      SYS-TIME  FROM      TIME.
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
*    Ｊ本田出荷確定データスタート
     MOVE     SPACE          TO   HDK-REC.
     INITIALIZE                   HDK-REC.
     MOVE     PARA-JDATE     TO   HDK-F011.
     MOVE     PARA-JTIME     TO   HDK-F012.
     MOVE     PARA-TORICD    TO   HDK-F013.
     MOVE     ZERO           TO   HDK-F03.
     MOVE     ZERO           TO   HDK-F04.
     MOVE     ZERO           TO   HDK-F06.
     START    HDSYUKF   KEY  >=   HDK-F01   HDK-F03
                                  HDK-F04   HDK-F06
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    Ｊ本田出荷確定データ読込み
     PERFORM HDSYUKF-READ-SEC.
*
     IF      END-FLG   =   ZERO
             MOVE     HDK-F011    TO   WK-HDK-F01
             MOVE     HDK-F012    TO   WK-HDK-F02
             MOVE     HDK-F013    TO   WK-HDK-F03
             MOVE     HDK-F03     TO   WK-HDK-F04
             MOVE     HDK-F04     TO   WK-HDK-F05
             MOVE     HDK-A05     TO   WK-HDK-A05
             MOVE     HDK-A06     TO   WK-HDK-A06
     END-IF.
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　Ｊ本田出荷確定データ読込み                          *
****************************************************************
 HDSYUKF-READ-SEC    SECTION.
*
     READ    HDSYUKF
         AT  END
             MOVE     "END"      TO  END-FLG
             GO                  TO  HDSYUKF-READ-EXIT
         NOT AT END
             ADD       1         TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  HDK-F011
     AND      PARA-JTIME  =  HDK-F012
     AND      PARA-TORICD =  HDK-F013
              CONTINUE
     ELSE
              MOVE     "END"     TO  END-FLG
              GO                 TO  HDSYUKF-READ-EXIT
     END-IF.
*    送信ＦＬＧのチェック
     IF       HDK-F091  =  "1"
              ADD     1          TO   SKIP1-CNT
              GO                 TO   HDSYUKF-READ-SEC
     END-IF.
*    抽出条件のチェック
*-----（ 倉庫 ）-----*
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF       PARA-SOKO   NOT =   HDK-F02
*-----        倉庫指定ありの時，倉庫ブレイクなら終了
******                 MOVE     "END"      TO   END-FLG
******                 GO                  TO   HDSYUKF-READ-EXIT
                       GO                  TO   HDSYUKF-READ-SEC
              END-IF
     END-IF.
*
*-----（ 納品日 ）-----*
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF       PARA-NOUDT   NOT =   HDK-F07
                       GO        TO   HDSYUKF-READ-SEC
              END-IF
     END-IF.
*
 HDSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*    店舗番号ブレイクチェック
     IF       HDK-F03  NOT =  WK-HDK-F04
*             ヘッダー１出力
              PERFORM  HDHEADF-H1-SEC
*             ヘッダー２出力
              PERFORM  HDHEADF-H2-SEC
              MOVE    HDK-F03     TO   WK-HDK-F04
              MOVE    HDK-F04     TO   WK-HDK-F05
              MOVE    ZERO        TO   WK-GOKEI
     END-IF.
*
*    伝票番号ブレイクチェック
     IF       HDK-F04  NOT =  WK-HDK-F05
*             ヘッダー２出力
              PERFORM  HDHEADF-H2-SEC
              MOVE    HDK-F04     TO   WK-HDK-F05
              MOVE    ZERO        TO   WK-GK-GENKA
     END-IF.
*
*    入荷予定ワーク（明細）出力
     PERFORM  HDHEADF-M1-SEC.
*
*    Ｊ本田出荷確定データ更新
     PERFORM  HDSYUKF-RWT-SEC.
*
*    Ｊ本田基本情報データ更新
     PERFORM  HDJOHOF-RWT-SEC.
*
*    Ｊ本田出荷確定データ読込み
     PERFORM HDSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　入荷予定データ　Ｆヘッダー１　作成処理　　　　　　　　　　*
****************************************************************
 HDHEADF-F1-SEC        SECTION.
*
     MOVE     "HDHEADF-F1-SEC  "  TO  S-NAME.
*
     MOVE     SPACE               TO  HNY-REC
                                      HNY-F1.
*
     MOVE     WK-HDK-F01          TO  HNY-F01.
     MOVE     WK-HDK-F02          TO  HNY-F02.
     MOVE     WK-HDK-F03          TO  HNY-F03.
     MOVE     ZERO                TO  HNY-F04
                                      HNY-F05
                                      HNY-F06.
*   レコード領域セット
     MOVE     "02"                TO  HNY-F1-A01.
     MOVE     "F1"                TO  HNY-F1-A02.
     MOVE     1                   TO  HNY-F1-A03.
     MOVE     LINK-OUT-YMD8       TO  HNY-F1-A04.
     MOVE     SYS-TIMEW           TO  HNY-F1-A05.
     MOVE     LINK-OUT-YMD8       TO  HNY-F1-A06.
     MOVE     SYS-TIMEW           TO  HNY-F1-A07.
     MOVE     SEQ-CNT             TO  HNY-F1-A08.
     MOVE     SPACE               TO  HNY-F1-A09.
     MOVE     WK-HDK-A05          TO  HNY-F1-A10.
     MOVE     WK-HDK-A06          TO  HNY-F1-A11.
*
     MOVE     HNY-F1              TO  HNY-F07.
*
     WRITE    HNY-REC.
     ADD      1                   TO  HNY-CNT.
*
 HDHEADF-F1-EXIT.
     EXIT.
****************************************************************
*　　入荷予定データ　ヘッダー１　作成処理　　　　　　　　　　　*
****************************************************************
 HDHEADF-H1-SEC        SECTION.
*
     MOVE     "HDHEADF-H1-SEC  "  TO   S-NAME.
*
     MOVE     SPACE               TO   HNY-REC.
*                                      HNY-H1.
     ADD      1                   TO   SEQ-CNT.
*
     MOVE     WK-HDK-F01          TO   HNY-F01.
     MOVE     WK-HDK-F02          TO   HNY-F02.
     MOVE     WK-HDK-F03          TO   HNY-F03.
     MOVE     WK-HDK-F04          TO   HNY-F04.
     MOVE     ZERO                TO   HNY-F05
                                       HNY-F06.
*   レコード領域セット
     MOVE     "02"                TO   HNY-H1-A01.
     MOVE     "H1"                TO   HNY-H1-A02.
*    MOVE     SEQ-CNT             TO   HNY-H1-A03.
     MOVE     ZERO                TO   HNY-H1-A03.
     MOVE     WK-HDK-A05          TO   HNY-H1-A04.
     MOVE     WK-HDK-A06          TO   HNY-H1-A05.
     MOVE     "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ"       TO   HNY-H1-A06.
     MOVE     900                 TO   HNY-H1-A08.
     MOVE     LINK-OUT-YMD8       TO   HNY-H1-A13.
     MOVE     ZERO                TO   HNY-H1-A14.
     MOVE     "02"                TO   HNY-H1-A16.
*
     MOVE     HNY-H1              TO   HNY-F07.
*
     WRITE    HNY-REC.
     ADD      1                   TO   HNY-CNT.
*
 HDHEADF-H1-EXIT.
     EXIT.
****************************************************************
*　　入荷予定データ　ヘッダー２　作成処理　　　　　　　　　　　*
****************************************************************
 HDHEADF-H2-SEC        SECTION.
*
     MOVE     "HDHEADF-H2-SEC"    TO   S-NAME.
*
     MOVE     SPACE               TO   HNY-REC.
*                                      HNY-H2.
     ADD      1                   TO   SEQ-CNT.
*
     MOVE     WK-HDK-F01          TO   HNY-F01.
     MOVE     WK-HDK-F02          TO   HNY-F02.
     MOVE     WK-HDK-F03          TO   HNY-F03.
     MOVE     WK-HDK-F04          TO   HNY-F04.
     MOVE     WK-HDK-F05          TO   HNY-F05.
     MOVE     ZERO                TO   HNY-F06.
*   レコード領域セット
     MOVE     "02"                TO   HNY-H2-A01.
     MOVE     "H2"                TO   HNY-H2-A02.
*    MOVE     SEQ-CNT             TO   HNY-H2-A03.
     MOVE     ZERO                TO   HNY-H2-A03.
     MOVE     WK-GK-GENKA         TO   HNY-H2-A09.
     MOVE     ZERO                TO   HNY-H2-A10.
     MOVE     "1"                 TO   HNY-H2-A11.
*
     MOVE     HNY-H2              TO   HNY-F07.
*
     WRITE    HNY-REC.
     ADD      1                   TO   HNY-CNT.
*
 HDHEADF-H2-EXIT.
     EXIT.
****************************************************************
*　　入荷予定データ　明　　細１　作成処理　　　　　　　　　　　*
****************************************************************
 HDHEADF-M1-SEC        SECTION.
*
     MOVE     "HDHEADF-M1-SE   "  TO   S-NAME.
*
     MOVE     SPACE               TO   HNY-REC.
     INITIALIZE                        HNY-H1   HNY-H2   HNY-M1.
     ADD      1                   TO   SEQ-CNT.
*
     MOVE     HDK-F011            TO   HNY-F01.
     MOVE     HDK-F012            TO   HNY-F02.
     MOVE     HDK-F013            TO   HNY-F03.
     MOVE     HDK-F03             TO   HNY-F04.
     MOVE     HDK-F04             TO   HNY-F05.
*
     MOVE     HDK-F06             TO   HNY-F06.
*   ヘッダー１領域セット*
     MOVE     HDK-A17             TO   HNY-H1-A07.
     MOVE     HDK-A12             TO   HNY-H1-A09.
     MOVE     HDK-A08             TO   HNY-H1-A10.
     MOVE     HDK-A10             TO   HNY-H1-A11.
     MOVE     HDK-A11             TO   HNY-H1-A12.
*   ヘッダー２領域セット*
     MOVE     HDK-A14             TO   HNY-H2-A04.
     MOVE     HDK-A15             TO   HNY-H2-A05.
     MOVE     HDK-A13             TO   HNY-H2-A06.
     MOVE     HDK-F05             TO   HNY-H2-A07.
     MOVE     HDK-F04             TO   HNY-H2-A08.
     MOVE     HDK-A19             TO   HNY-H2-A12.
*   明細レコード領域セット
     MOVE     "02"                TO   HNY-M1-A01.
     MOVE     "M1"                TO   HNY-M1-A02.
*    MOVE     SEQ-CNT             TO   HNY-M1-A03.
     MOVE     ZERO                TO   HNY-M1-A03.
     IF       HDK-F10   =   ZERO
              MOVE      "04"      TO   HNY-M1-A04
     ELSE
              MOVE      "00"      TO   HNY-M1-A04
     END-IF.
     MOVE     HDK-F06             TO   HNY-M1-A05.
     MOVE     HDK-F06             TO   HNY-M1-A06.
     MOVE     HDK-A25             TO   HNY-M1-A07.
     MOVE     HDK-A26             TO   HNY-M1-A08.
     MOVE     HDK-A29             TO   HNY-M1-A09.
     MOVE     HDK-A31             TO   HNY-M1-A10.
     MOVE     HDK-F10             TO   HNY-M1-A11.
*原価単価取得
     MOVE     SPACE               TO   HSHOTBL-INV-FLG.
     MOVE     HDK-F013            TO   TBL-F01.
     MOVE     HDK-A26             TO   TBL-F02.
     READ     HSHOTBL
         INVALID
              MOVE    "INV"       TO   HSHOTBL-INV-FLG
         NOT  INVALID
              MOVE    SPACE       TO   HSHOTBL-INV-FLG
     END-READ.
     IF       HSHOTBL-INV-FLG   =   SPACE
              MOVE     TBL-F05    TO   WK-HNY-M1-A12
              MOVE WK-HNY-M1-A12  TO   HNY-M1-A12
     END-IF.
     MOVE     ZERO                TO   HNY-M1-A13
                                       HNY-M1-A14
                                       HNY-M1-A15.
* 集計
*     ADD      HDK-A31             TO   WK-GK-SURYO.
*****COMPUTE  WK-GK-GENKA = WK-GK-GENKA + (HDK-A31 * TBL-F05).
     COMPUTE  WK-GK-GENKA = WK-GK-GENKA + (HDK-F10 * TBL-F05).
*
     MOVE     HNY-M1              TO  HNY-F07.
*
     WRITE    HNY-REC.
     ADD      1                   TO   HNY-CNT.
*
 HDHEADF-M1-EXIT.
     EXIT.
****************************************************************
*　　Ｊ本田出荷確定データ更新                                  *
****************************************************************
 HDSYUKF-RWT-SEC        SECTION.
*
     MOVE     "HDSYUKF-RWT-SEC"   TO   S-NAME.
*
     MOVE     "1"                 TO   HDK-F091.
     MOVE     LINK-OUT-YMD8       TO   HDK-F092.
*
     REWRITE  HDK-REC.
     ADD      1                   TO   HDK-CNT.
*
 HDSYUKF-RWT-EXIT.
     EXIT.
****************************************************************
*　　Ｊ本田基本情報データ更新                                  *
****************************************************************
 HDJOHOF-RWT-SEC        SECTION.
*
     MOVE     "HDJOHOF-RWT-SEC"   TO   S-NAME.
*
     MOVE     HDK-F01             TO   HDJ-F01.
     MOVE     HDK-F04             TO   HDJ-F04.
     MOVE     HDK-F06             TO   HDJ-F06.
     READ     HDJOHOF
         INVALID
              GO                  TO   HDJOHOF-RWT-EXIT
     END-READ.
*
     MOVE     "1"                 TO   HDJ-F091.
     MOVE     LINK-OUT-YMD8       TO   HDJ-F092.
*
     REWRITE  HDJ-REC.
     ADD      1                   TO   HDJ-CNT.
*
 HDJOHOF-RWT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     IF       SEQ-CNT   >   1
              PERFORM  HDHEADF-H1-SEC
              PERFORM  HDHEADF-H2-SEC
              PERFORM  HDHEADF-F1-SEC
     END-IF.
*
     DISPLAY "Ｊ本田確定 READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "Ｊ本田 送信済み CNT = " SKIP1-CNT UPON CONS.
     DISPLAY "Ｊ本田確定 REWT CNT = " HDK-CNT   UPON CONS.
     DISPLAY "Ｊ本田情報 REWT CNT = " HDJ-CNT   UPON CONS.
     DISPLAY "入荷予定Ｗ   WT CNT = " HNY-CNT   UPON CONS.
*
     CLOSE     HSHOTBL  HDJOHOF  HDSYUKF  HDHEADF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
