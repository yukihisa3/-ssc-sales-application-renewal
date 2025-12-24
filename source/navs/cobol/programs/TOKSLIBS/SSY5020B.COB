****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　トステムビバ　　　　　　　　　　　
*    業務名　　　　　　　：　新ＥＤＩオンラインシステム　　　　
*    モジュール名　　　　：　納品予定データ作成　　　　　
*    作成日／更新日　　　：　2010/03/30
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　受け取ったパラメタより対象データ
*                            を出荷情報保存データに抽出する。　
*    更新履歴            ：
*      2011/10/07 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/03/30.
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
*（共通）基本情報ファイル
     SELECT   KHJOHOF   ASSIGN    TO        DA-01-VI-KHJOHOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KHJ-F01   KHJ-F02
                                            KHJ-F03   KHJ-F04
                                            KHJ-F08   KHJ-F05
                                            KHJ-F06   KHJ-F07
                        FILE      STATUS    KHJ-STATUS.
*（共通）出荷情報ファイル
     SELECT   KHSYUKF   ASSIGN    TO        DA-01-VI-KHSYUKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KHS-F01   KHS-F02
                                            KHS-F03   KHS-F04
                                            KHS-F08   KHS-F05
                                            KHS-F06   KHS-F07
                        FILE      STATUS    KHS-STATUS.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*売上伝票ファイル
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*（共通）基本情報ファイル
******************************************************************
 FD  KHJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     KHJOHOF   OF        XFDLIB
              JOINING   KHJ       PREFIX.
******************************************************************
*（基本）出荷情報ファイル
******************************************************************
 FD  KHSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     KHSYUKF   OF        XFDLIB
              JOINING   KHS       PREFIX.
******************************************************************
*条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  DEN-STATUS          PIC  X(02).
     03  KHS-STATUS          PIC  X(02).
     03  KHJ-STATUS          PIC  X(02).
     03  JYO-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP1-CNT           PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  KOS1-CNT            PIC  9(08)     VALUE  ZERO.
     03  WK-GYO-CNT          PIC  9(01)     VALUE  ZERO.
*INVALID領域
 01  WK-INV-FLG.
     03  KHSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  HJYOKEN-INV-FLG     PIC  X(03)     VALUE  SPACE.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY5020B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5020B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5020B".
         05  FILLER          PIC  X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06)  VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(07)  VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-IN.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(09)  VALUE " INPUT = ".
         05  IN-CNT          PIC  9(06).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT         PIC  9(06).
         05  FILLER          PIC  X(05)  VALUE " *** ".
*ブレイクＫＥＹ領域
 01  WK-BREAK-KEY.
     03  WK-JDATE            PIC  9(08)   VALUE  ZERO.
     03  WK-JTIME            PIC  9(04)   VALUE  ZERO.
     03  WK-TOKCD            PIC  9(08)   VALUE  ZERO.
     03  WK-SOKCD            PIC  X(02)   VALUE  SPACE.
     03  WK-NOUDT            PIC  9(08)   VALUE  ZERO.
     03  WK-TENCD            PIC  9(05)   VALUE  ZERO.
     03  WK-DENNO            PIC  9(09)   VALUE  ZERO.
     03  WK-GYO              PIC  9(02)   VALUE  ZERO.
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE              PIC   9(08).
 01  PARA-JTIME              PIC   9(04).
 01  PARA-TORICD             PIC   9(08).
 01  PARA-SOKO               PIC   X(02).
 01  PARA-NOUDT              PIC   9(08).
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
                        PROCEDURE   KHJOHOF.
     MOVE      "KHJOHOL3"   TO   AB-FILE.
     MOVE      KHJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KHSYUKF.
     MOVE      "KHSYUKL3"   TO   AB-FILE.
     MOVE      KHS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
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
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
     STOP  RUN
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       KHJOHOF  SHTDENF.
     OPEN     I-O       KHSYUKF.
     OPEN     INPUT     HJYOKEN.
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
*（共通）基本情報ファイルＳＴＡＲＴ
     MOVE     SPACE          TO      KHJ-REC.
     INITIALIZE                      KHJ-REC.
     MOVE     PARA-JDATE     TO      KHJ-F01.
     MOVE     PARA-JTIME     TO      KHJ-F02.
     MOVE     PARA-TORICD    TO      KHJ-F03.
     MOVE     PARA-SOKO      TO      KHJ-F04.
     MOVE     PARA-NOUDT     TO      KHJ-F08.
     MOVE     ZERO           TO      KHJ-F05.
     MOVE     ZERO           TO      KHJ-F06.
     MOVE     ZERO           TO      KHJ-F07.
     START    KHJOHOF   KEY  >=      KHJ-F01   KHJ-F02
                                     KHJ-F03   KHJ-F04
                                     KHJ-F08   KHJ-F05
                                     KHJ-F06   KHJ-F07
         INVALID   KEY
              DISPLAY "＃＃対象データ無し＃＃" UPON CONS
              DISPLAY  MSG-END UPON CONS
              STOP  RUN
     END-START.
*（共通）基本情報ファイル読込
     PERFORM KHJOHOF-READ-SEC.
*対象が存在した場合は、ＫＥＹをワークに退避する。
*（１行目に行数を更新するため）
     IF   END-FLG  NOT =  "END"
          MOVE   KHJ-F01     TO      WK-JDATE
          MOVE   KHJ-F02     TO      WK-JTIME
          MOVE   KHJ-F03     TO      WK-TOKCD
          MOVE   KHJ-F04     TO      WK-SOKCD
          MOVE   KHJ-F08     TO      WK-NOUDT
          MOVE   KHJ-F05     TO      WK-TENCD
          MOVE   KHJ-F06     TO      WK-DENNO
          MOVE   KHJ-F07     TO      WK-GYO
     ELSE
          DISPLAY "＃＃対象データ無し＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KHJOHOF-READ-SEC    SECTION.
*
     READ     KHJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KHJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  KHJ-F01
     AND      PARA-JTIME  =  KHJ-F02
     AND      PARA-TORICD =  KHJ-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  KHJOHOF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  KHJ-F03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  KHJOHOF-READ-EXIT
              END-IF
     END-IF.
*    送信ＦＬＧのチェック
     IF       KHJ-F11  =  "1"
              GO                 TO   KHJOHOF-READ-SEC
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  KHJ-F04
                   CONTINUE
              ELSE
                   GO            TO  KHJOHOF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  KHJ-F08
                  CONTINUE
              ELSE
                  GO             TO  KHJOHOF-READ-SEC
              END-IF
     END-IF.
*
 KHJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     KHJ-F03             TO   DEN-F01.
     MOVE     KHJ-F06             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     KHJ-F07             TO   DEN-F03.
* 2011/10/07,S  S.I/NAV
     MOVE  KHJ-F05          TO  DEN-F07.  *> 店舗CD
     MOVE  KHJ-F16          TO  DEN-F112. *> 納品日
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
              MOVE     SPACE      TO   KHS-REC
              INITIALIZE               KHS-REC
              MOVE    KHJ-F01     TO   KHS-F01
              MOVE    KHJ-F02     TO   KHS-F02
              MOVE    KHJ-F03     TO   KHS-F03
              MOVE    KHJ-F04     TO   KHS-F04
              MOVE    KHJ-F08     TO   KHS-F08
              MOVE    KHJ-F05     TO   KHS-F05
              MOVE    KHJ-F06     TO   KHS-F06
              MOVE    KHJ-F07     TO   KHS-F07
              PERFORM   KHSYUKF-READ-SEC
              IF    KHSYUKF-INV-FLG = "INV"
                    PERFORM   KHSYUKF-WRITE-SEC
                    MOVE   "1"    TO   KHJ-F09
                    MOVE  SYS-DATEW  TO   KHJ-F10
                    REWRITE  KHJ-REC
              END-IF
     END-IF.
 MAIN010.
*    ＤＣＭＪＡＰＡＮ発注情報保存データ読込み
     PERFORM KHJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*出荷情報作成セクション
****************************************************************
 KHSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "KHSYUKF-WRITE-SEC"  TO  S-NAME.
*
     IF    WK-JDATE  =  KHJ-F01
     AND   WK-JTIME  =  KHJ-F02
     AND   WK-TOKCD  =  KHJ-F03
     AND   WK-SOKCD  =  KHJ-F04
     AND   WK-NOUDT  =  KHJ-F08
     AND   WK-TENCD  =  KHJ-F05
     AND   WK-DENNO  =  KHJ-F06
           CONTINUE
     ELSE
*          ＫＥＹがブレイクした場合、１行目に明細行数をセット
           MOVE     SPACE      TO   KHS-REC
           INITIALIZE               KHS-REC
           MOVE    WK-JDATE    TO   KHS-F01
           MOVE    WK-JTIME    TO   KHS-F02
           MOVE    WK-TOKCD    TO   KHS-F03
           MOVE    WK-SOKCD    TO   KHS-F04
           MOVE    WK-NOUDT    TO   KHS-F08
           MOVE    WK-TENCD    TO   KHS-F05
           MOVE    WK-DENNO    TO   KHS-F06
           MOVE    1           TO   KHS-F07
           PERFORM   KHSYUKF-READ-SEC
           IF    KHSYUKF-INV-FLG  NOT =  "INV"
                 MOVE  WK-GYO-CNT  TO  KHS-F17
                 REWRITE  KHS-REC
           END-IF
           MOVE    KHJ-F01         TO  WK-JDATE
           MOVE    KHJ-F02         TO  WK-JTIME
           MOVE    KHJ-F03         TO  WK-TOKCD
           MOVE    KHJ-F04         TO  WK-SOKCD
           MOVE    KHJ-F08         TO  WK-NOUDT
           MOVE    KHJ-F05         TO  WK-TENCD
           MOVE    KHJ-F06         TO  WK-DENNO
           MOVE    KHJ-F07         TO  WK-GYO
     END-IF.
*
     MOVE      SPACE               TO  KHS-REC.
     INITIALIZE                        KHS-REC.
*基本情報のレコードをセットする。
     MOVE      KHJ-REC             TO  KHS-REC.
*出荷情報項目に項目をセットする。
*    出荷日（処理日をセットする。）
     MOVE      SYS-DATEW(3:6)      TO  KHS-HD19.
*    納品予定日（納品日をセットする。）
     MOVE      KHJ-F08(3:6)        TO  KHS-HD20.
*    出荷数量
     MOVE      DEN-F15             TO  KHS-ME07.
*    原価金額
     COMPUTE   KHS-ME13  =  KHS-ME07  * ( KHS-ME11 / 100 ).
*    売価金額
     COMPUTE   KHS-ME14  =  KHS-ME07  *  KHS-ME12.
*    メーカー区分セット
*    条件ファイルに存在するかチェックする。
     PERFORM   HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = "INV"
         MOVE  SPACE               TO  KHS-ME15  KHS-ME16
     ELSE
         MOVE  "1"                 TO  KHS-ME15
         MOVE  JYO-F14             TO  KHS-ME16
     END-IF.
*空白
     MOVE      SPACE               TO  KHS-ME17  KHS-ME21.
*欠品数量
     MOVE      ZERO                TO  KHS-ME19.
*****2011/05/11 NAV ST
*****
*****COMPUTE   KHS-ME19  =  DEN-F50 -  DEN-F15.
     COMPUTE   KHS-ME19  =  KHJ-ME09 -  DEN-F15.
*****2011/05/11 NAV ED
     IF        KHS-ME19  >  ZERO
***************IF   KHS-ME15  =  "1"
*                   MOVE  "22"     TO  KHS-ME20
*              ELSE
*                   MOVE  "21"     TO  KHS-ME20
***************END-IF
***************EVALUATE  DEN-F31
               EVALUATE  DEN-F411
                   WHEN  "1" MOVE "11"  TO  KHS-ME20
                   WHEN  "2" MOVE "12"  TO  KHS-ME20
                   WHEN  "3" MOVE "21"  TO  KHS-ME20
                   WHEN  "4" MOVE "22"  TO  KHS-ME20
                   WHEN  OTHER
                         IF   KHS-ME15  =  "1"
                              MOVE  "22"     TO  KHS-ME20
                         ELSE
                              MOVE  "21"     TO  KHS-ME20
                         END-IF
               END-EVALUATE
     ELSE
               MOVE ZERO           TO  KHS-ME19
***************MOVE SPACE          TO  KHS-ME20
*2010/5/27 NAV 欠品なし時、”００”をセット START
               MOVE "00"           TO  KHS-ME20
*2010/5/27 NAV 欠品なし時、”００”をセット END
     END-IF.
*行番号をワークに格納する。
     MOVE      KHJ-F07             TO  WK-GYO-CNT.
*
     WRITE     KHS-REC.
     ADD       1                   TO  KOS1-CNT.
*
 KHSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     IF    KOS1-CNT  >  ZERO
           MOVE     SPACE      TO   KHS-REC
           INITIALIZE               KHS-REC
           MOVE    WK-JDATE    TO   KHS-F01
           MOVE    WK-JTIME    TO   KHS-F02
           MOVE    WK-TOKCD    TO   KHS-F03
           MOVE    WK-SOKCD    TO   KHS-F04
           MOVE    WK-NOUDT    TO   KHS-F08
           MOVE    WK-TENCD    TO   KHS-F05
           MOVE    WK-DENNO    TO   KHS-F06
           MOVE    1           TO   KHS-F07
           PERFORM   KHSYUKF-READ-SEC
           IF    KHSYUKF-INV-FLG  NOT =  "INV"
                 MOVE  WK-GYO-CNT  TO   KHS-F17
                 REWRITE  KHS-REC
           END-IF
     END-IF.
*
     DISPLAY "KHJOHOF READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ   CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "KHSYUKF WT   CNT = " KOS1-CNT  UPON CONS.
*
     CLOSE     SHTDENF  KHJOHOF  KHSYUKF  HJYOKEN.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KHSYUKF-READ-SEC    SECTION.
*
     READ     KHSYUKF    INVALID
              MOVE    "INV"       TO   KHSYUKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   KHSYUKF-INV-FLG
     END-READ.
*
 KHSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*（共通）条件ファイル読込
****************************************************************
 HJYOKEN-READ-SEC    SECTION.
*
     MOVE     "95"           TO   JYO-F01.
     MOVE     DEN-F1411      TO   JYO-F02.
     READ     HJYOKEN    INVALID
              MOVE    "INV"       TO   HJYOKEN-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
