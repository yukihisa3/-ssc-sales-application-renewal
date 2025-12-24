****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｊ本田オンラインシステム　　　　　*
*    業務名　　　　　　　：　Ｊ本田オンラインシステム　　　　　*
*    モジュール名　　　　：　出荷確定データ作成                *
*    作成日／更新日　　　：　2005/05/18                        *
*    作成者／更新者　　　：　C FUJIWARA                        *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を基本情報データより抽出する。    *
*    更新日／更新者　　　：　2011/10/11 /YOSHIDA.M             *
*    更新概要　　　　　　：　基幹サーバ統合                    *
*    更新日／更新者　　　：　2020/02/03 /T.TAKAHASHI           *
*    更新概要　　　　　　：　売上伝票Ｆ原価単価セット          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7104B.
 AUTHOR.                C FUJIWARA.
 DATE-WRITTEN.          05/05/18.
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
***2011.10.11 ST
                                            DEN-F07   DEN-F112
***2011.10.11 EN
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*Ｊ本田基本情報データ
     SELECT   HDJOHOF   ASSIGN    TO        DA-01-VI-HDJOHOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HDJ-F01   HDJ-F02
                                            HDJ-F07   HDJ-F03
                                            HDJ-F04   HDJ-F06
                        FILE      STATUS    HDJ-STATUS.
*Ｊ本田出荷確定データ
     SELECT   HDSYUKF   ASSIGN    TO        DA-01-VI-HDSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HDK-F01   HDK-F02
                                            HDK-F04   HDK-F06
                        FILE      STATUS    HDK-STATUS.
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
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP1-CNT           PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  HDK1-CNT            PIC  9(08)     VALUE  ZERO.
     03  HDK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  HDJ-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  HDSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-HDJ-F04              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HDJ-STATUS        PIC  X(02).
     03  HDK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7104B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7104B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7104B".
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
     OPEN     I-O       HDJOHOF  HDSYUKF.
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
*    Ｊ本田基本情報データスタート
     MOVE     SPACE          TO   HDJ-REC.
     INITIALIZE                   HDJ-REC.
     MOVE     PARA-JDATE     TO   HDJ-F011.
     MOVE     PARA-JTIME     TO   HDJ-F012.
     MOVE     PARA-TORICD    TO   HDJ-F013.
     MOVE     PARA-SOKO      TO   HDJ-F02.
     MOVE     PARA-NOUDT     TO   HDJ-F07.
     MOVE     ZERO           TO   HDJ-F03.
     MOVE     ZERO           TO   HDJ-F04.
     MOVE     ZERO           TO   HDJ-F06.
     START    HDJOHOF   KEY  >=   HDJ-F01   HDJ-F02
                                  HDJ-F07   HDJ-F03
                                  HDJ-F04   HDJ-F06
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    Ｊ本田基本情報データ読込み
     PERFORM HDJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　Ｊ本田基本情報データ読込み                          *
****************************************************************
 HDJOHOF-READ-SEC    SECTION.
*
     READ    HDJOHOF
         AT  END
             MOVE     "END"      TO  END-FLG
             GO                  TO  HDJOHOF-READ-EXIT
         NOT AT END
             ADD       1         TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  HDJ-F011
     AND      PARA-JTIME  =  HDJ-F012
     AND      PARA-TORICD =  HDJ-F013
              CONTINUE
     ELSE
              MOVE     "END"     TO  END-FLG
              GO                 TO  HDJOHOF-READ-EXIT
     END-IF.
*    送信ＦＬＧのチェック
     IF       HDJ-F091  =  "1"
              ADD     1          TO   SKIP1-CNT
              GO                 TO   HDJOHOF-READ-SEC
     END-IF.
*    抽出条件のチェック
*-----（ 倉庫 ）-----*
     IF       PARA-SOKO   =  SPACE
              CONTINUE
*             GO                 TO   HDJOHOF-READ-EXIT
     ELSE
              IF       PARA-SOKO   NOT =   HDJ-F02
*-----        倉庫指定ありの時，倉庫ブレイクなら終了
                       MOVE     "END"      TO   END-FLG
                       GO                  TO   HDJOHOF-READ-EXIT
*                      GO        TO   HDJOHOF-READ-SEC
              END-IF
     END-IF.
*
*-----（ 納品日 ）-----*
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF       PARA-NOUDT   NOT =   HDJ-F07
                       GO        TO   HDJOHOF-READ-SEC
              END-IF
     END-IF.
*
 HDJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*    伝票番号ブレイクチェック
     IF       HDJ-F04  NOT =  WK-HDJ-F04
              MOVE    ZERO        TO   WK-GYO-CNT
              MOVE    HDJ-F04     TO   WK-HDJ-F04
     END-IF.
*
*    売上伝票ファイル検索
     MOVE     HDJ-F013            TO   DEN-F01.
     MOVE     HDJ-F04             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     HDJ-F06             TO   DEN-F03.
***2011.10.11 ST
     MOVE     HDJ-F03             TO   DEN-F07.
     MOVE     HDJ-F07             TO   DEN-F112.
***2011.10.11 EN
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
*------------ 数量確認
**************IF   DEN-F15  NOT =  ZERO
                   PERFORM   HDSYUKF-WRITE-SEC
**************ELSE
*----------------- 数量＝０の時は、送信済とする。
*****              MOVE      "1"                 TO  HDJ-F081
*****              MOVE      LINK-OUT-YMD8       TO  HDJ-F082
*****              MOVE      "1"                 TO  HDJ-F091
*****              MOVE      LINK-OUT-YMD8       TO  HDJ-F092
*****              REWRITE   HDJ-REC
*****              ADD       1                   TO  HDJ-CNT
*****         END-IF
     END-IF.
 MAIN010.
*    Ｊ本田基本情報データ読込み
     PERFORM HDJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　Ｊ本田出荷確定データ作成処理　　　　　　　　　　　　　　　*
****************************************************************
 HDSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "HDSYUKF-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  HDK-REC.
     INITIALIZE                        HDK-REC.
     MOVE      HDJ-F01             TO  HDK-F01.
     MOVE      HDJ-F02             TO  HDK-F02.
     MOVE      HDJ-F03             TO  HDK-F03.
     MOVE      HDJ-F04             TO  HDK-F04.
     MOVE      HDJ-F05             TO  HDK-F05.
*--- 行番号の変更
*    MOVE      HDJ-F06             TO  HDK-F06.
     ADD       1                   TO  WK-GYO-CNT.
     MOVE      WK-GYO-CNT          TO  HDK-F06.
     MOVE      HDJ-F07             TO  HDK-F07.
     MOVE      "1"                 TO  HDK-F081.
     MOVE      LINK-OUT-YMD8       TO  HDK-F082.
     MOVE      DEN-F15             TO  HDK-F10.
*
*--- ヘッダレコード情報
     MOVE      HDJ-A01             TO  HDK-A01.
     MOVE      HDJ-A02             TO  HDK-A02.
     MOVE      HDJ-A03             TO  HDK-A03.
     MOVE      HDJ-A04             TO  HDK-A04.
     MOVE      HDJ-A05             TO  HDK-A05.
     MOVE      HDJ-A06             TO  HDK-A06.
     MOVE      HDJ-A07             TO  HDK-A07.
     MOVE      HDJ-A08             TO  HDK-A08.
     MOVE      HDJ-A09             TO  HDK-A09.
     MOVE      HDJ-A10             TO  HDK-A10.
     MOVE      HDJ-A11             TO  HDK-A11.
     MOVE      HDJ-A12             TO  HDK-A12.
     MOVE      HDJ-A13             TO  HDK-A13.
     MOVE      HDJ-A14             TO  HDK-A14.
     MOVE      HDJ-A15             TO  HDK-A15.
     MOVE      HDJ-A16             TO  HDK-A16.
     MOVE      HDJ-A17             TO  HDK-A17.
     MOVE      HDJ-A18             TO  HDK-A18.
     MOVE      HDJ-A19             TO  HDK-A19.
     MOVE      HDJ-A20             TO  HDK-A20.
*--- 明細レコード情報
     MOVE      HDJ-A21             TO  HDK-A21.
     MOVE      HDJ-A22             TO  HDK-A22.
     MOVE      HDJ-A23             TO  HDK-A23.
     MOVE      HDJ-A24             TO  HDK-A24.
     MOVE      HDJ-A25             TO  HDK-A25.
     MOVE      HDJ-A26             TO  HDK-A26.
     MOVE      HDJ-A27             TO  HDK-A27.
     MOVE      HDJ-A28             TO  HDK-A28.
     MOVE      HDJ-A29             TO  HDK-A29.
     MOVE      HDJ-A30             TO  HDK-A30.
     MOVE      HDJ-A31             TO  HDK-A31.
*#2020/02/03 NAV ST 売上伝票Ｆの原価単価をセットに変更する。
*****MOVE      HDJ-A32             TO  HDK-A32.
     MOVE      DEN-F172            TO  HDK-A32.
*#2020/02/03 NAV ED 売上伝票Ｆの原価単価をセットに変更する。
     MOVE      HDJ-A33             TO  HDK-A33.
     MOVE      HDJ-A34             TO  HDK-A34.
*
*-----（基本情報データ 出荷確定データ作成情報セット）-----*
     MOVE      "1"                 TO  HDJ-F081.
     MOVE      LINK-OUT-YMD8       TO  HDJ-F082.
*
     WRITE     HDK-REC.
     ADD       1                   TO  HDK1-CNT.
     REWRITE   HDJ-REC.
     ADD       1                   TO  HDJ-CNT.
*
 HDSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "Ｊ本田情報 READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "Ｊ本田 送信済み CNT = " SKIP1-CNT UPON CONS.
     DISPLAY "売上DT 無し     CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "Ｊ本田確定DT WT CNT = " HDK1-CNT  UPON CONS.
     DISPLAY "Ｊ本田情報 REWT CNT = " HDJ-CNT   UPON CONS.
*     DISPLAY "Ｊ本田確定 REWT CNT = " HDK2-CNT  UPON CONS.
*
     CLOSE     SHTDENF  HDJOHOF  HDSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
